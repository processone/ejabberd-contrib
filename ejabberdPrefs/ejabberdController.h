//
//  ejabberdController.h
//  ejabberd preference pane
//
//  Created on Wed Apr 19 2006.
//  Copyright (c) 2006-2009 ProcessOne.
//

#import <Cocoa/Cocoa.h>

@interface ejabberdController : NSObject
{
    IBOutlet NSTextField *status;
	//IBOutlet NSImageView *image;
	IBOutlet NSImageView *imagestarted;
	IBOutlet NSImageView *imagestopped;
	IBOutlet NSProgressIndicator *actionProgress;
	IBOutlet NSButton *startStopButton;
	IBOutlet NSButton *automaticBox;
	BOOL started;
	char startscript[128];
	char stopscript[128];
	char statusscript[128];
	char waitstartedscript[128];
	char waitstoppedscript[128];
}
- (void)setStarted:(BOOL)flag;
- (BOOL)isStarted;
- (void)getRunningStatus;
- (void)waitRunningStatus;
- (void)updateRunningStatus;
- (IBAction)startStopAction:(id)sender;
- (IBAction)automaticStartAction:(id)sender;
@end
