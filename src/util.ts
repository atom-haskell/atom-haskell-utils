// tslint:disable: no-null-keyword
import { Directory, File, TextBuffer } from 'atom'
import * as fs from 'fs'
import * as path from 'path'

function hasGetPath(dir: any): dir is Directory | File {
  return dir && dir.getPath && typeof dir.getPath === 'function'
}

function isTextBuffer(x: any): x is TextBuffer {
  return x && x.file
}

export function isDirectory(
  dir: File | Directory | string | null | any,
): boolean {
  if (dir === null) {
    return false
  }
  if (hasGetPath(dir)) {
    return isDirectory(dir.getPath())
  } else if (typeof dir === 'string') {
    try {
      return fs.statSync(dir).isDirectory()
    } catch (e) {
      return false
    }
  } else {
    return false
  }
}

export function getRootDirFallback(file: File | Directory | null): Directory {
  let dir: Directory | null = null
  if (file) {
    ;[dir] = atom.project
      .getDirectories()
      .filter((d) => d.contains(file.getPath()))
  }
  if (!dir) {
    dir = atom.project.getDirectories()[0]
  }
  if (dir && dir.getPath() === 'atom://config') {
    dir = null
  }
  if (!(dir && isDirectory(dir))) {
    if (file) {
      dir = file.getParent()
    } else {
      dir = new Directory('.')
    }
  }
  return dir
}

export async function getDirEntries(
  dir: Directory,
): Promise<Array<Directory | File>> {
  return new Promise<Array<Directory | File>>((resolve, reject) =>
    dir.getEntries((error, contents) => {
      if (error) {
        reject(error)
      } else {
        resolve(contents)
      }
    }),
  )
}

async function dirHasCabalFile(d: Directory) {
  if (!d) {
    return false
  }
  return (await getDirEntries(d)).some(
    (file) => file.isFile() && file.getBaseName().endsWith('.cabal'),
  )
}

async function dirHasSandboxFile(d: Directory) {
  if (!d) {
    return false
  }
  return (await getDirEntries(d)).some(
    (file) => file.isFile() && file.getBaseName() === 'cabal.sandbox.config',
  )
}

async function findProjectRoot(
  d: Directory,
  check: (d: Directory) => Promise<boolean>,
) {
  while (!(d.isRoot() || (await check(d)))) {
    d = d && d.getParent()
  }
  if (await check(d)) {
    return d
  } else {
    return null
  }
}

export async function getRootDir(
  input: TextBuffer | File | string | null,
): Promise<Directory> {
  let file: File | Directory | null
  if (isTextBuffer(input)) {
    const p = input.getPath()
    if (p) file = new File(p)
    else file = null
  } else if (hasGetPath(input)) {
    file = input
  } else if (typeof input === 'string') {
    file = new File(input)
  } else {
    file = null
  }
  let dir: Directory | null
  if (file && isDirectory(file)) {
    dir = new Directory(file.getPath())
  } else {
    dir = (file && file.getParent()) || getRootDirFallback(file)
  }
  const cabalRoot = await findProjectRoot(dir, dirHasCabalFile)
  const sandboxRoot = await findProjectRoot(dir, dirHasSandboxFile)
  dir = cabalRoot || sandboxRoot
  if (!(dir && isDirectory(dir))) {
    dir = getRootDirFallback(file)
  }
  return dir
}

import HS = require('../hs/hs.min.js')
export { ITarget, IDotCabal, IImport, IModuleImports } from '../hs/hs.min.js'

import CP = require('child_process')
const cabal2jsonPath = process.env.ATOM_HASKELL_CABAL2JSONPATH
  ? process.env.ATOM_HASKELL_CABAL2JSONPATH
  : path.join(
      __dirname,
      '..',
      'bin',
      'cabal2json-' +
        process.platform +
        (process.platform === 'win32' ? '.exe' : ''),
    )

async function runCabal2Json<T>(cabalSource: string, args: string[], def: T) {
  return new Promise<T>((resolve) => {
    const cp = CP.execFile(cabal2jsonPath, args, function (
      error,
      stdout,
      _stderr,
    ) {
      if (error) {
        atom.notifications.addError(
          'Atom-Haskell core error in getComponentFromFile',
          {
            detail: error.message,
            dismissable: true,
          },
        )
        resolve(def)
      } else {
        resolve(JSON.parse(stdout))
      }
    })
    try {
      cp.stdin.write(cabalSource, 'utf8')
      cp.stdin.end()
    } catch (e) {
      atom.notifications.addError(
        'Atom-Haskell core error in getComponentFromFile',
        {
          detail: e.message,
          dismissable: true,
        },
      )
      try {
        cp.kill()
      } catch (e2) {}
    }
  })
}

export async function parseDotCabal(cabalSource: string) {
  return runCabal2Json<HS.IDotCabal | null>(cabalSource, [], null)
}
export async function getComponentFromFile(
  cabalSource: string,
  filePath: string,
) {
  return runCabal2Json<string[]>(cabalSource, [filePath], [])
}
export async function unlit(filename: string, source: string) {
  return new Promise<string>((resolve, reject) => {
    HS.unlit(filename, source, (error, result) => {
      if (error) {
        reject(new Error(error))
      } else if (result) {
        resolve(result)
      } else {
        reject(new Error('Unknown error when trying to run unlit'))
      }
    })
  })
}
function isErrorResult(x: any): x is { error: string } {
  return x && x.error && typeof x.error === 'string'
}
export async function parseHsModuleImports(source: string) {
  return new Promise<HS.IModuleImports>((resolve, reject) => {
    HS.parseHsModuleImports(source, (result) => {
      if (isErrorResult(result)) {
        reject(new Error(result.error))
      } else {
        resolve(result)
      }
    })
  })
}
export let { hsEscapeString } = HS
