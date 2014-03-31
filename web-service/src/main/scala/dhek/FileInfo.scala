package dhek

import java.io.FileInputStream

import resource.ManagedResource

case class FileInfo(filename: String, file: ManagedResource[FileInputStream])
