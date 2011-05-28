{-# LANGUAGE CPP #-}
module Distribution.Compat.Filesystem
  (
   pathSeparator,
   pathSeparators,
   pathCoerceToDirectory,
   pathCoerceToFile,
   pathIsAbsolute,
   pathIsRelative,
   pathIsDirectory,
   pathIsFile,
   (</>),
   removeFileSilently,
   removeDirectorySilently,
   removeDirectoryRecursiveSilently,
   listDirectory
  )
  where


import Distribution.Compat.Filesystem.Portable
#ifdef VERSION_unix
import Distribution.Compat.Filesystem.Posix
#else
import Distribution.Compat.Filesystem.Windows
#endif
