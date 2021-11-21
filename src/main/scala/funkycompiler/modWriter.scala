package funkycompiler

import scala.language.unsafeNulls

import org.w3c.dom.*
import org.xml.sax.InputSource
import javax.xml.parsers.*
import javax.xml.transform.*;
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.{ StreamResult, StreamSource }
import java.io.*


def writeVariables(file: File, variables: String): Unit =
  val docBldr = DocumentBuilderFactory.newInstance().newDocumentBuilder()
  val doc: Document = docBldr.parse(file)
  val varDoc: Document = docBldr.parse(InputSource(StringReader(variables)))
  val oldVars: Node = doc.getElementsByTagName("Variables").item(0)
  val root = oldVars.getParentNode()
  val newVars: Node = doc.importNode(varDoc.getDocumentElement(), true)
  root.insertBefore(newVars, oldVars)
  root.removeChild(oldVars)
  writeXml(doc, file)

def writeXml(doc: Document, file: File): Unit =
  val transformer = TransformerFactory.newInstance().newTransformer()
  transformer.setOutputProperty(OutputKeys.INDENT, "yes")
  transformer.setOutputProperty(OutputKeys.STANDALONE, "no")
  transformer.transform(DOMSource(doc), StreamResult(file))
