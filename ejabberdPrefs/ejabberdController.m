//
//  ejabberdController.m
//  ejabberd preference pane
//
//  Created on Wed Apr 19 2006.
//  Copyright (c) 2006-2009 ProcessOne.
//


#import "ejabberdController.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

@implementation ejabberdController

- (id) init
{
	if((self = [super init])) {
		[self performSelector:@selector(getRunningStatus) withObject:self afterDelay:0.5];
		/*
		[automaticBox setState:1];
		[automaticBox setNeedsDisplay];
		NSDictionary *defaultDefaults = [[NSDictionary alloc] initWithContentsOfFile:
			[bundle pathForResource:@"ejabberdPath" ofType:@"plist"]];
		[[EjabberdPreferences preferences] registerDefaults:defaultDefaults];
		[defaultDefaults release];
		 */
		//TODO: This is very dirty code. need rewrite by MacOS/ObjC coder.
		FILE *file;
		char conf[255];
		strcpy(conf, getenv("HOME"));
		strcat(conf, "/Library/PreferencePanes/ejabberd.prefPane/Contents/Resources/config.txt");
		file = fopen(conf,"r");
		char path[255], *ptr;
		memset(path, 0, 128);
		fgets(path, 127, file);
		for(ptr=path; *ptr; ptr++)
		{
			if(*ptr==10) *ptr=0;
			if(*ptr==13) *ptr=0;
		}
		fclose(file);
		sprintf(startscript, "%s/bin/ejabberdctl start", path);
		sprintf(stopscript, "%s/bin/ejabberdctl stop", path);
		sprintf(statusscript, "%s/bin/ejabberdctl status", path);
		sprintf(waitstartedscript, "%s/bin/ejabberdctl started", path);
		sprintf(waitstoppedscript, "%s/bin/ejabberdctl stopped", path);
	}
	return self;
}

- (void) setStarted:(BOOL)flag
{
	started = flag;
}

- (BOOL) isStarted
{
	return started;
}

- (void) getRunningStatus
{
	[actionProgress startAnimation:self];
	started = (system(statusscript) == 0);
	[self updateRunningStatus];
}

- (void) waitRunningStatus
{
	[actionProgress startAnimation:self];
	system(started?waitstartedscript:waitstoppedscript);
	[self getRunningStatus];
}

- (void) updateRunningStatus
{
	//[startStopButton setTitle:isStarted
	//	? NSLocalizedStringFromTableInBundle(@"Stop ejabberd",nil,bundle,@"")
	//	: NSLocalizedStringFromTableInBundle(@"Start ejabberd",nil,bundle,@"")];
	if(started)
	{
		[startStopButton setTitle:@"Stop ejabberd"];
		[status setStringValue:@"ejabberd is started."];
		[imagestarted setHidden:NO];
		[imagestopped setHidden:YES];
	} else {
		[startStopButton setTitle:@"Start ejabberd"];
		[status setStringValue:@"ejabberd is stopped."];
		[imagestarted setHidden:YES];
		[imagestopped setHidden:NO];
	}
	[actionProgress stopAnimation:self];
}

-(IBAction)startStopAction:(id)sender
{
	[actionProgress startAnimation:self];
	if(started)
	{
		[status setStringValue:@"Stopping ejabberd..."];
		started = !(system(stopscript) == 0);
	} else {
		[status setStringValue:@"Starting ejabberd..."];
		started = (system(startscript) == 0);
	}
	[self performSelector:@selector(waitRunningStatus) withObject:self afterDelay:0.5];
}

-(IBAction)automaticStartAction:(id)sender
{
	//TODO: implement autostart
	if([automaticBox state])
	{
		system("mkdir -p ~/Library/LaunchDaemons");
		system("cp /Library/PreferencePanes/ejabberd.prefPane/Contents/Resources/ejabberd.plist ~/Library/LaunchDaemons");
		system("launchctl load ~/Library/LaunchDaemons/ejabberd.plist");
	} else {
		system("launchctl unload ~/Library/LaunchDaemons/ejabberd.plist");
		system("rm ~/Library/LaunchDaemons/ejabberd.plist");
	}
}

@end
