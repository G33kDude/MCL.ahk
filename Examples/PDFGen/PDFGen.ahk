#Include %A_ScriptDir%/../../
#Include MCLib.ahk

SetWorkingDir, % A_ScriptDir

FileRead, Source, PDFGenToAHK.c

PDFGen := MCLib.FromC(Source)

DllCall(PDFGen.pdf_get_a4_dimensions, "float*", A4Width, "float*", A4Height)

pDocument := DllCall(PDFGen.pdf_create_info
    , "AStr", "AutoHotkey"
    , "AStr", "MCLib & pdfgen"
    , "AStr", "Example PDF"
    , "AStr", "CloakerSmoker"
    , "AStr", "Creating PDF files from AutoHotkey using a C library"
    , "AStr", "2021-06-26"
    , "Float", A4Width, "Float", A4Height
    , "Ptr")

DllCall(PDFGen.pdf_set_font, "Ptr", pDocument, "AStr", "Times-Roman")

pPage := DllCall(PDFGen.pdf_append_page, "Ptr", pDocument, "Ptr")

DllCall(PDFGen.pdf_add_text_wrap, "Ptr", pDocument, "Ptr", pPage
    , "AStr", "This is text (inside a PDF)"
    , "Float", 24, "Float", 60, "Float", 800
    , "Int", 0x000000
    , "Float", 300, "Int", 2, "Ptr", 0)
DllCall(PDFGen.pdf_add_line, "Ptr", pDocument, "Ptr", pPage
    , "Float", 25, "Float", 750, "Float", A4Width - 25, "Float", 750, "Float", 3
    , "Int", 0x000000)
DllCall(PDFGen.pdf_add_text_wrap, "Ptr", pDocument, "Ptr", pPage
    , "AStr", "It was made by AHK calling a C library dynamically loaded as mcode. I pray for death."
    , "Float", 16, "Float", 60, "Float", 700
    , "Int", 0x000000
    , "Float", 300, "Int", 2, "Ptr", 0)

DllCall(PDFGen.pdf_save, "Ptr", pDocument, "AStr", "output.pdf")
DllCall(PDFGen.pdf_destroy, "Ptr", pDocument)

MsgBox, % "Done"