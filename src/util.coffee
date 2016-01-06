{Directory, File} = require 'atom'
fs = require 'fs'
HS = -> require '../hs/HaskellCabal.min.js'

module.exports = Util =
  isDirectory: (dir) ->
    switch
      when dir instanceof Directory
        return fs.statSync(dir.getPath()).isDirectory()
      when dir instanceof File
        return fs.statSync(dir.getPath()).isDirectory()
      when typeof dir is 'string'
        return fs.statSync(dir).isDirectory()
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
        when bufferOrFileOrString instanceof File
          bufferOrFileOrString
        when typeof bufferOrFileOrString is 'string'
          new File(bufferOrFileOrString)
    dir = file?.getParent?() ? Util.getRootDirFallback file
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
