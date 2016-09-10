package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.android.widgets.ShadowButton;

import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;

public class SimpleTextInputDialog extends Dialog 
{
	public enum ButtonType
	{
		NORMAL, SHADOW;
	}
	
	
	
	public interface DialogButtonListener
	{
		public void onAccept(String text);
		public void onCancel();
	}
	
	ButtonType _buttonType = ButtonType.NORMAL;
	

	
	String _message;
	String _title;
	DialogButtonListener _onAcceptClickListener;
	
	public SimpleTextInputDialog(Context con, DialogButtonListener acceptListener, String title, String message)
    {
    	super(con);
    	_onAcceptClickListener = acceptListener;
    	_message = message;
    	_title = title;
    }
	
    public SimpleTextInputDialog(Context con, DialogButtonListener acceptListener, String title, String message, ButtonType buttonType)
    {
    	super(con);
    	_buttonType = buttonType;
    	_onAcceptClickListener = acceptListener;
    	_message = message;
    	_title = title;
    	
    }
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);
		setContentView(R.layout.simple_text_input);
		configure();
	}
	
	private void configure()
	{
		View acceptButton;
		View cancelButton;
		LinearLayout top = (LinearLayout)findViewById(R.id.lin_button_container);
		final EditText input = (EditText)findViewById(R.id.edit_simple_dialog_input);
		TextView messageText = (TextView)findViewById(R.id.txt_simple_dialog_message);
		messageText.setText(_message);
		setTitle(_title);
		View removedContainer;
		if (_buttonType == ButtonType.NORMAL)
		{
			removedContainer = findViewById(R.id.lin_simple_dialog_shadow_buttons);
		}
		else
		{
			removedContainer = findViewById(R.id.lin_simple_dialog_normal_buttons);
		}
		top.removeView(removedContainer);
		acceptButton = top.findViewById(R.id.simple_text_button_accept);
		acceptButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				_onAcceptClickListener.onAccept(input.getText().toString());
				dismiss();
			}
		});		
		cancelButton = top.findViewById(R.id.simple_text_button_cancel);
		cancelButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				dismiss();
				_onAcceptClickListener.onCancel();
			}
		});
		
	}
	
}
