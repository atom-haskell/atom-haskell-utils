{Directory, File} = require 'atom'
fs = require 'fs'
HS = -> require '../hs/hs.min.js'

module.exports = Util =
  isDirectory: (dir) ->
    switch
      when typeof dir.getPath is 'function'
        return Util.isDirectory(dir.getPath())
      when typeof dir is 'string'
        return ((try fs.statSync(dir).isDirectory()) ? false)
      else
        return false

  getRootDirFallback: (file) ->
    [dir] = atom.project.getDirectories().filter (dir) ->
      dir.contains(file?.getPath?())
    unless dir?
      dir = atom.project.getDirectories()[0]
    if dir?.getPath?() is 'atom://config'
      dir = null
    unless Util.isDirectory(dir)
      dir = file?.getParent?() ? new Directory '.'
    dir

  getRootDir: (bufferOrFileOrString) ->
    dirHasCabalFile = (d) ->
      return false unless d?
      d.getEntriesSync().some (file) ->
        file.isFile() and file.getBaseName().endsWith '.cabal'
    dirHasSandboxFile = (d) ->
      return false unless d?
      d.getEntriesSync().some (file) ->
        file.isFile() and (file.getBaseName() is 'cabal.sandbox.config')
    findProjectRoot = (d, check) ->
      until d?.isRoot?() or not d? or check d
        d = d?.getParent?()
      d if check d
    file =
      switch
        when bufferOrFileOrString.file?
          bufferOrFileOrString.file
        when typeof bufferOrFileOrString.getPath is 'function'
          bufferOrFileOrString
        when typeof bufferOrFileOrString is 'string'
          new File(bufferOrFileOrString)
    dir =
      if Util.isDirectory(file)
        new Directory(file.getPath())
      else
        file?.getParent?() ? Util.getRootDirFallback file
    dir = findProjectRoot(dir, dirHasCabalFile) ? findProjectRoot(dir, dirHasSandboxFile)
    unless Util.isDirectory(dir)
      dir = Util.getRootDirFallback file
    return dir

  parseDotCabal: (a...) -> HS().parseDotCabal(a...)
  getComponentFromFile: (a...) -> HS().getComponentFromFile(a...)
  parseDotCabalSync: (a...) -> HS().parseDotCabalSync(a...)
  getComponentFromFileSync: (a...) -> HS().getComponentFromFileSync(a...)
  unlit: (a...) -> HS().unlit(a...)
  unlitSync: (a...) -> HS().unlitSync(a...)
  parseHsModuleImports: (a...) -> HS().parseHsModuleImports(a...)
  parseHsModuleImportsSync: (a...) -> HS().parseHsModuleImportsSync(a...)
