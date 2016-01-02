{Directory, File} = require 'atom'
HS = -> require '../hs/HaskellCabal.min.js'

module.exports = Util =
  getRootDirFallback: (file) ->
    [dir] = atom.project.getDirectories().filter (dir) ->
      dir.contains(file?.getPath?())
    unless dir?
      dir = atom.project.getDirectories()[0]
    if dir?.getPath?() is 'atom://config'
      dir = null
    unless dir?.isDirectory?()
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
      if bufferOrFileOrString.file?
        bufferOrFileOrString.file?
      else if bufferOrFileOrString instanceof File
        bufferOrFileOrString
      else if typeof bufferOrFileOrString is 'string'
        new File(bufferOrFileOrString)
    dir = file?.getParent?() ? Util.getRootDirFallback file
    dir = findProjectRoot(dir, dirHasCabalFile) ? findProjectRoot(dir, dirHasSandboxFile)
    unless dir?.isDirectory?()
      dir = Util.getRootDirFallback file
    return dir

  parseDotCabal: (a...) -> HS().parseDotCabal(a...)
  getComponentFromFile: (a...) -> HS().getComponentFromFile(a...)
