
			// File : /okcontent/js/service/help.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/service/help.js";
			
var Help={type:null,showSection:function(section){var sections=$('main_column').select('div.section');for(var i=0;i<sections.length;i++){if(sections[i].id.endsWith(section)){sections[i].show();$(sections[i].id.sub('_','_li_')).addClassName('active');}
else{sections[i].hide();$(sections[i].id.sub('_','_li_')).removeClassName('active');}}
util.adjustMCHeight();documentOffset=document.viewport.getScrollOffsets().top;contentOffset=$('main_content').cumulativeOffset().top;if(documentOffset>contentOffset){$('main_content').scrollTo();}},checkFeedback:function(){var type=$F('fb_type'),subject=$F('fb_subject'),text=$F('fb_text'),email=$F('fb_email'),oldtype=this.type;this.type=type;if(type!=oldtype){$('fb_clarifications').immediateDescendants().invoke('hide');$('fb_clarifications').hide();switch(type){case'Events':$('fb_fields').show();$('fb_clarifications_events').show();$('fb_clarifications').show();return;break;case'SuccessStory':$('fb_fields').hide();$('fb_clarifications_success').show();$('fb_clarifications').show();return;break;case'BusinessInquiry':$('fb_fields').hide();$('fb_clarifications_business').show();$('fb_clarifications').show();return;break;case'Billing':$('fb_fields').show();$('fb_clarifications_billing').show();$('fb_clarifications').show();break;case'Remove':$('fb_fields').hide();$('fb_clarifications_remove').show();$('fb_clarifications').show();break;case'Report':$('fb_fields').hide();$('fb_clarifications_report').show();$('fb_clarifications').show();return;break;default:$('fb_fields').show();break;}}
if(!type.blank()&&!subject.blank()&&!text.blank()&&!email.blank()){$('feedback_submit').removeClassName('gray').addClassName('green');$('feedback_errors').hide();}
else
$('feedback_submit').removeClassName('green').addClassName('gray');},sendFeedback:function(){var isuploading=false;var uploader=$('okupload-iframe-feedback_upload');if(uploader){if(uploader.contentWindow.$('okupload-prompt-upload-feedback_upload').visible())
isuploading=true;if(uploader.contentWindow.$('okupload-prompt-import-feedback_upload').visible())
isuploading=true;}
if($('uploadwarn'))isuploading=false;var errors='';if($F('fb_type').blank()){errors+='<li>Select an issue type</li>';}
if($F('fb_subject').blank()){errors+='<li>Fill in a subject</li>';}
if($F('fb_text').blank()){errors+='<li>Fill in your message</li>';}
if($F('fb_email').blank()){errors+='<li>Please include an email address</li>';}
if(isuploading){errors+='<li id="uploadwarn">Click "Attach" to add your screenshot. If you don&rsquo;t want to attach a screenshot, click "Send" again.</li>';}
if(errors!=''){$('feedback_errors').update(errors).show();}
else{$('feedback_errors').hide();var text=$F('fb_text');$('fb_text_full').value=text;$('feedback_form').submit();}},showUploader:function(){$('photo_upload_area').show();var photo_cb=function(up){if(up&&up.Status=='success'){$('photo_url').value=up.ResultUrl;$('photo_upload_area').update('<img src="http://akcdn.okccdn.com/php/load_okc_image.php/images/160x160/160x160/6x75/1066x1134/2/'+up.Picid+'.jpeg" width="160" height="160"/>');$('photo_note_text').update('You\'ve included an image.');}};var ct=$F('fb_type')=='success'?12:18;var photo_upper=getSimpleUploader('photo_upload_area',photo_cb,{TempId:"feedback_upload",ShouldCommit:true,CommitStatus:ct,IFrameParams:"style='width:490px'",IFrameStyleTemplate:'locals/photoupload.css'});$('photo_note_text').update('Do you have a photo or screenshot that would help us?');$('yes_photo_button').innerHTML='<em>(start over)</em>';},fillBrowserInfo:function(current_time){var toolbar=document.cookie.match('toolbar=installed')?'Yes':'No';var now=new Date();var br=new Array(4);var os=new Array(2);var flash=new Array(2);br=getBrowser();os=getOS();flash=hasFlashPlugin();jsver=jsVersion();viewport=document.viewport.getDimensions();var client_info='\n----------------\n'
+'Time diff: '+(now.getTime()/1000-current_time)+'seconds;\n'
+'Browser time: '+now+' ('+now.getTime()+') [Server = '+current_time+']\n'
+'Browser identifier: '+br[0]+'\n'
+'Browser version: '+br[1]+'\n'
+'Browser major version: '+getMajorVersion(br[1])+'\n'
+'Browser minor version: '+getMinorVersion(br[1])+'\n'
+'Browser engine: '+br[2]+'\n'
+'Browser engine version: '+br[3]+'\n'
+'Full user agent string: '+getFullUAString()+'\n'
+'Operating system identifier: '+os[0]+'\n'
+'Operating system version: '+os[1]+'\n'
+'Is Flash installed? '+(flash[0]==2?'Yes':(flash[0]==1?'No':'unknown'))+'\n'
+'Flash version: '+flash[1]+'\n'
+'What is the newest version of Javascript this browser supports? '+jsver+'\n'
+'Viewport: '+viewport.width+'x'+viewport.height+'\n';+'------------------';$('browser_info').value=client_info;}};
			AUTOCORE_SELF_CHECK = null;
		
addNewCoresToCookie(["83"], "44f0be9a");