{
    ApplicationDescription = "GNU Emacs for GNUstep / OS X";
    ApplicationIcon = emacs.tiff;
    ApplicationName = Emacs;
    ApplicationRelease = "24.2";
    Authors = (
	"Adrian Robert (GNUstep)",
	"Christophe de Dinechin (MacOS X)",
	"Scott Bender (OpenStep)",
	"Christian Limpach (NeXTstep)",
	"Carl Edman (NeXTstep)",
	"..see etc/NEXTSTEP"
    );
    Copyright = "Copyright (C) 2012 Free Software Foundation, Inc.";
    CopyrightDescription = "Released under the GNU General Public License Version 3 or later";
    FullVersionID = "Emacs 24.2, NS Windowing";
    NSExecutable = Emacs;
    NSIcon = emacs.tiff;
    NSPrincipalClass = NSApplication;
    NSRole = Application;
    NSTypes = (
	{
	    NSDocumentClass = "";
	    NSHumanReadableName = "";
	    NSIcon = "";
	    NSName = "";
	    NSRole = "";
	    NSUnixExtensions = (
		txt
	    );
	},
	{
	    NSDocumentClass = "";
	    NSHumanReadableName = "";
	    NSIcon = "";
	    NSName = "";
	    NSRole = "";
	    NSUnixExtensions = (
		c,
		h
	    );
	},
	{
	    NSDocumentClass = "";
	    NSHumanReadableName = "";
	    NSIcon = "";
	    NSName = "";
	    NSRole = "";
	    NSUnixExtensions = (
		m
	    );
	},
	{
	    NSDocumentClass = "";
	    NSHumanReadableName = "";
	    NSIcon = "";
	    NSName = "";
	    NSRole = "";
	    NSUnixExtensions = (
		C,
		cpp,
		H,
		cc
	    );
	},
	{
	    NSDocumentClass = "";
	    NSHumanReadableName = "";
	    NSIcon = "";
	    NSName = "";
	    NSRole = "";
	    NSUnixExtensions = (
		java
	    );
	},
	{
	    NSDocumentClass = "";
	    NSHumanReadableName = "";
	    NSIcon = "";
	    NSName = "";
	    NSRole = "";
	    NSUnixExtensions = (
		el
	    );
	},
	{
	    NSDocumentClass = "";
	    NSHumanReadableName = "";
	    NSIcon = "";
	    NSName = "";
	    NSRole = "";
	    NSUnixExtensions = (
		*
	    );
	}
    );
    NSServices = (
	{
	    NSPortName = Emacs;
	    NSMessage = requestService;
	    NSUserData = open-selection;
	    NSSendTypes = (NSStringPboardType);
	    NSMenuItem = {
		default = "Emacs.app/New Buffer Containing Selection";
	    };
	},
	{
	    NSPortName = Emacs;
	    NSMessage = requestService;
	    NSUserData = open-file;
	    NSSendTypes = (NSStringPboardType);
	    NSMenuItem = {
		default = "Emacs.app/Open Selected File";
	    };
	},
	{
	    NSPortName = Emacs;
	    NSMessage = requestService;
	    NSUserData = mail-selection;
	    NSSendTypes = (NSStringPboardType);
	    NSMenuItem = {
		default = "Emacs.app/Email Selection";
	    };
	},
	{
	    NSPortName = Emacs;
	    NSMessage = requestService;
	    NSUserData = mail-to;
	    NSSendTypes = (NSStringPboardType)