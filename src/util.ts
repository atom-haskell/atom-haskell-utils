// tslint:disable: no-null-keyword no-var-requires
import { Directory, File, TextBuffer } from 'atom'
import * as fs from 'fs'

function hasGetPath(dir): dir is Directory | File {
  return dir && dir.getPath && typeof dir.getPath === 'function'
}

function isTextBuffer(x): x is TextBuffer {
  return x && x.file
}

export function isDirectory(dir: File | Directory | string | null | any): boolean {
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
    [dir] = atom.project.getDirectories().filter((d) => d.contains(file.getPath()))
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

export async function getDirEntries(dir: Directory): Promise<Array<Directory | File>> {
  return new Promise<Array<Directory | File>>((resolve, reject) => dir.getEntries((error, contents) => {
    if (error) {
      reject(error)
    } else {
      resolve(contents)
    }
  }))
}

export async function getRootDir(input: TextBuffer | File | string | null): Promise<Directory> {
  async function dirHasCabalFile(d: Directory) {
    if (!d) { return false }
    return (await getDirEntries(d)).some((file) => file.isFile() && file.getBaseName().endsWith('.cabal'))
  }
  async function dirHasSandboxFile(d: Directory) {
    if (!d) { return false }
    return (await getDirEntries(d)).some((file) => file.isFile() && file.getBaseName() === 'cabal.sandbox.config')
  }
  async function findProjectRoot(d: Directory, check: (d: Directory) => Promise<boolean>) {
    while (!(d && d.isRoot && d.isRoot() || !d || await check(d))) {
      d = d && d.getParent()
    }
    if (await check(d)) {
      return d
    } else {
      return null
    }
  }
  let file: File | Directory | null
  if (isTextBuffer(input)) {
    file = new File(input.getPath())
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
    dir = file && file.getParent() || getRootDirFallback(file)
  }
  const cabalRoot = await findProjectRoot(dir, dirHasCabalFile)
  const sandboxRoot = await findProjectRoot(dir, dirHasSandboxFile)
  dir = cabalRoot || sandboxRoot
  if (!(dir && isDirectory(dir))) {
    dir = getRootDirFallback(file)
  }
  return dir
}

const HS = require('../hs/hs.min.js') as IHS

export async function parseDotCabal(cabalSource: string): Promise<IDotCabal | null> {
  return new Promise<IDotCabal | null>((resolve) => {
    HS.parseDotCabal(cabalSource, resolve)
  })
}
export async function getComponentFromFile(cabalSource: string, filePath: string): Promise<string[]> {
  return new Promise<string[]>((resolve) => {
    HS.getComponentFromFile(cabalSource, filePath, resolve)
  })
}
export async function unlit(filename: string, source: string): Promise<string> {
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
function isErrorResult(x): x is { error: string } {
  return x && x.error && typeof x.error === 'string'
}
export async function parseHsModuleImports(source: string): Promise<IModuleImports> {
  return new Promise<IModuleImports>((resolve, reject) => {
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

export interface ITarget {
  type: 'library' | 'executable' | 'test-suite' | 'benchmark'
  name: string
  target: string
}

export interface IDotCabal {
  name: string
  version: string
  targets: ITarget[]
}

export interface IImport {
  name: string
  qualified: boolean
  hiding: boolean
  importList: null | Array<string | { parent: string }>
  alias: null | string
}

export interface IModuleImports {
  name: string
  imports: IImport[]
}

export interface IHS {
  parseDotCabal(cabalSource: string, callback: (result: IDotCabal | null) => void): void
  parseDotCabalSync(cabalSource: string): IDotCabal
  getComponentFromFile(cabalSource: string, filePath: string, callback: (result: string[]) => void): void
  getComponentFromFileSync(cabalSource: string, filePath: string): string[]
  unlit(
    filename: string, source: string,
    callback: (
      error: null | string,
      result: null | string,
    ) => void,
  ): void
  unlitSync(filename: string, source: string): string | { error: string }
  parseHsModuleImports(source: string, callback: (result: { error: string } | IModuleImports) => void): void
  parseHsModuleImportsSync(source: string): IModuleImports
  hsEscapeString(input: string): string
}
