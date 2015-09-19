package com.evolved.automata.desktop;
import java.awt.Color;

import javax.swing.*;
import javax.swing.text.*;

public class AIInfoDisplay {
	JTextArea TextArea;
	JScrollPane ScrollPane;
	JFrame TextFrame;
	public AIInfoDisplay(int rows, int columns,String description)
	{
		TextArea = new JTextArea(rows,columns);
		ScrollPane = new JScrollPane(TextArea);
		TextArea.setEditable(false);
		TextFrame = new JFrame(description);
		TextFrame.getContentPane().add(ScrollPane);
		TextFrame.pack();
		TextFrame.setVisible(true);
		TextArea.setBackground(Color.BLACK);
		TextArea.setForeground(Color.WHITE);
		
	}
	
	public void setTitle(String title)
	{
		TextFrame.setTitle(title);
	}
	
	public void dismiss()
	{
		TextFrame.dispose();
		//TextFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}
	
	public AIInfoDisplay(String description)
	{
		TextArea = new JTextArea(20,80);
		ScrollPane = new JScrollPane(TextArea);
		TextArea.setEditable(false);
		TextFrame = new JFrame(description);
		TextFrame.getContentPane().add(ScrollPane);
		TextFrame.pack();
		TextFrame.setVisible(true);
		TextArea.setBackground(Color.BLACK);
		TextArea.setForeground(Color.WHITE);
		
	}
	
	
	public void DisplayMessage(String message)
	{
		TextArea.append(message);
		JScrollBar sbar =  ScrollPane.getVerticalScrollBar();
		sbar.setValue(sbar.getMaximum());
	}
	
	public void DisplayMessage(String message,boolean popupP)
	{
		TextArea.append(message);
		if (popupP)
		{
			TextFrame.setAlwaysOnTop(true);
			TextFrame.setAlwaysOnTop(false);
		}
		JScrollBar sbar =  ScrollPane.getVerticalScrollBar();
		sbar.setValue(sbar.getMaximum());
	}
	
	public void DisplayMessageLine(String message)
	{
		DisplayMessage(message+"\n");
	}
	
	public void DisplayMessageLine(String message,boolean popupP)
	{
		DisplayMessage(message+"\n",popupP);
	}
	
}
