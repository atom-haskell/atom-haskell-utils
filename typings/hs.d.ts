declare namespace HS {
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
    importList: null | Array<string | {parent: string}>
    alias: null | string
  }

  export interface IModuleImports {
    name: string
    imports: IImport[]
  }

  export interface IHS {
    parseDotCabal (cabalSource: string, callback: (result: IDotCabal | null) => void): void
    parseDotCabalSync (cabalSource: string): IDotCabal
    getComponentFromFile (cabalSource: string, filePath: string, callback: (result: string[]) => void): void
    getComponentFromFileSync (cabalSource: string, filePath: string): string[]
    unlit (
      filename: string, source: string,
      callback: (
        error: null | string,
        result: null | string,
      ) => void,
    ): void
    unlitSync (filename: string, source: string): string | {error: string}
    parseHsModuleImports (source: string, callback: (result: {error: string} | IModuleImports) => void): void
    parseHsModuleImportsSync (source: string): IModuleImports
    hsEscapeString (input: string): string
  }
}
