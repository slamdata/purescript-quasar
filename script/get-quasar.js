var https = require("https");
var fs = require("fs");
var cp = require("child_process");
var rimraf = require("rimraf");
var path = require("path");
var config = require("../quasar-versions.json");

const checkQuasarVersion = (config) =>
  new Promise((resolve, reject) => {
    if (fs.existsSync("./quasar/quasar.jar")) {
      const proc = cp.spawn("java", "-jar quasar/quasar.jar --help".split(" "));
      const version = config.tag.replace(/v?([0-9\.]+)/, "$1");

      const out = [];
      proc.stdout.on("data", (d) => out.push(d));
      proc.stdout.on("end", () => {
        const value = Buffer.concat(out).toString();
        if (err.length == 0) resolve(value.indexOf("quasar " + version) != -1);
      })

      const err = [];
      proc.stderr.on("data", (d) => err.push(d));
      proc.stderr.on("end", () => {
        if (err.length > 0) reject(new Error(Buffer.concat(err).toString()));
      });

    } else {
      resolve(false)
    }
  });

const githubRequestHeaders = (token) =>
  ({ "User-Agent": "GitHubAPI", "Authorization": "token " + token });

const fetchReleaseInfo = (options, token) =>
  new Promise ((resolve, reject) => {
    const requestOptions =
      { host: "api.github.com"
      , headers: githubRequestHeaders(token)
      , path: "/repos/" + options.owner + "/" + options.repo + "/releases/tags/" + options.tag
      };
    https.get(requestOptions, (response) => {
      var acc = "";
      response.on("data", (chunk) => acc += chunk);
      response.on("error", (err) => reject(err));
      response.on("end", () => {
        try { resolve(JSON.parse(acc)); }
        catch (exn) { reject(exn); }
      });
    });
  });

const update = (options, token) => {
  console.log("Updating for Quasar " + options.tag);
  return fetchReleaseInfo(options, token)
    .then(info => {
      const result = { jar: null, plugins: [] };
      return fetchQuasar(options, info, token)
        .then(file => {
          result.jar = file;
          return fetchPlugins(options, info, token);
        })
        .then(files => {
          result.plugins = files;
          return result;
        });
    })
    .then(result => {
      console.log("Copying files for updated version...");
      rimraf.sync("./quasar/plugins")
      tryMkdir("./quasar/plugins")
      rimraf.sync("./quasar/quasar.jar")
      return Promise.all([copyFile(result.jar, "./quasar/quasar.jar")]
        .concat(result.plugins.map(jar => copyFile(jar, "./quasar/plugins/" + path.basename(jar)))));
    })
    .then(() => "Updated to Quasar " + options.tag);
  }

const copyFile = (src, dest) =>
  new Promise((resolve, reject) => {
    fs.createReadStream(src)
      .on("error", err => reject(err))
      .on("end", () => resolve(dest))
      .pipe(fs.createWriteStream(dest));
  });

const fetchQuasar = (options, info, token) => {
  const asset = info.assets.filter(function (asset) {
    return asset.name.indexOf(options.prefix) !== -1
  })[0];
  if (asset == undefined) {
    reject("No asset found for the main Quasar .jar");
    return;
  }
  return fetchAsset(options, asset, token);
};

const fetchPlugins = (options, info, token) => {
  const files = [];
  const assets = info.assets.filter(function (asset) {
    return asset.name.indexOf(options["plugin-string"]) !== -1
  });
  const loop = () =>
    assets.length == 0
      ? files
      : fetchAsset(options, assets.shift(), token)
          .then((file) => {
            files.push(file);
            return loop();
          });
  return loop();
};

const fetchAsset = (options, asset, token) =>
  new Promise((resolve, reject) => {
    const tempFile = "./quasar/cache/" + asset.name
    try {
      const stats = fs.statSync(tempFile);

      if (asset.size == stats.size) {
        console.info("Reusing cached asset for " + asset.name);
        resolve(tempFile);
        return;
      }
    } catch (exn) {
      if (exn.code != "ENOENT") reject(exn);
    };

    const requestHeaders = githubRequestHeaders(token);
    requestHeaders["Accept"] = "application/octet-stream";

    const requestOptions =
      { host: "api.github.com"
      , headers: requestHeaders
      , path: "/repos/" + options.owner + "/" + options.repo + "/releases/assets/" + asset.id
      };

    https.get(requestOptions, (response) => {

      const assetLocation = response.headers.location;
      if (assetLocation == undefined) {
        reject("Asset location not found");
        return;
      }
      https.get(assetLocation, (assetResponse) => {
        const contentLength = parseInt(assetResponse.headers["content-length"], 10);
        var downloaded = 0;
        var lastUpdate = 0;
        assetResponse.on("data", (chunk) => {
          downloaded += chunk.length;
          const now = Date.now();
          if (now - lastUpdate > 500) {
            const percent = Math.round(downloaded / contentLength * 10000) / 100;
            process.stdout.clearLine();
            process.stdout.cursorTo(0);
            process.stdout.write("Downloading " + asset.name + "... " + percent + "%");
            lastUpdate = now;
          }
        });
        assetResponse.on("end", (chunk) => {
          process.stdout.clearLine();
          process.stdout.cursorTo(0);
          console.log ("Downloaded " + asset.name);
        });
        const destFile = fs.createWriteStream(tempFile);
        assetResponse.pipe(destFile);
        destFile.on("finish", () => resolve(tempFile));
      });
    });
  });

const tryMkdir = (path) => {
  try {
    fs.mkdirSync(path);
  } catch (exn) {
    if (exn.code != "EXIST") throw exn;
  }
};

const main = () => {
  tryMkdir("./quasar");
  tryMkdir("./quasar/cache");

  const token = process.env.GITHUB_AUTH_TOKEN;
  const quasarConfig = config["quasar"];

  checkQuasarVersion(quasarConfig)
    .then(isOk =>
      isOk === true
        ? "Existing version is up to date for Quasar " + quasarConfig.tag
        : update(quasarConfig, token))
    .catch(err => {
      console.warn("Updating Quasar, an error occurred when checking the existing version:\n\n\t" + err.message.replace(/\n/, "\n\t") + "\n");
      return update(quasarConfig, token);
    })
    .catch(err => console.error("Failed to fetch quasar", err))
    .then(msg => console.log("Done!", msg));
};

main();
