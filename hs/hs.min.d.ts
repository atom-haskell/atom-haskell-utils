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

export function parseDotCabal(
  cabalSource: string,
  callback: (result: IDotCabal | null) => void,
): void
export function parseDotCabalSync(cabalSource: string): IDotCabal
export function getComponentFromFile(
  cabalSource: string,
  filePath: string,
  callback: (result: string[]) => void,
): void
export function getComponentFromFileSync(
  cabalSource: string,
  filePath: string,
): string[]
export function unlit(
  filename: string,
  source: string,
  callback: (error: null | string, result: null | string) => void,
): void
export function unlitSync(
  filename: string,
  source: string,
): string | { error: string }
export function parseHsModuleImports(
  source: string,
  callback: (result: { error: string } | IModuleImports) => void,
): void
export function parseHsModuleImportsSync(source: string): IModuleImports
export function hsEscapeString(input: string): string
