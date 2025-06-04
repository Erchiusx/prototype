import { opendir } from "fs/promises";
import { resolve } from "path";


const wd = process.argv.at(-1)
process.cwd = resolve(wd);

const collection = [];

function getpath({ path, name }) {
  return `${path}/${name}`
}

async function gather(dir, prefix = "") {
  const dirinfo = await opendir(resolve(process.cwd, getpath(dir ?? { path: ".", name: "" })));
  for await (let fd of dirinfo) {
    if (fd.isFile()) {
      if (fd.name.endsWith(".hs"))
        collection.push(`${prefix}.${fd.name.slice(0, -3)}`);
      if (fd.name.endsWith(".lhs"))
        collection.push(`${prefix}.${fd.name.slice(0, -4)}`)
    }
    else
      await gather(fd, `${prefix}${prefix ? '.' : ''}${fd.name}`)
  }
}

await gather();
for (let mod of collection)
  console.log(mod)
