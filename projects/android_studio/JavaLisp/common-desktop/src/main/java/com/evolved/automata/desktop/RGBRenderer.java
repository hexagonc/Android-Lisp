package com.evolved.automata.desktop;
import javax.swing.*;
import java.awt.*;


public class RGBRenderer extends JComponent{

	int m_Width;
	int m_Height;
	int[] m_Image;
	boolean m_RepaintOnImageUpdate;
	static int m_Counter=0;
	
	public RGBRenderer(int width, int height, int[] image)
	{
		m_Width=width;
		m_Height=height;
		m_Image=image;
		//setSize(width,height);
		setPreferredSize(new java.awt.Dimension(m_Width,m_Height));
		m_RepaintOnImageUpdate=false;
	}

	public RGBRenderer(int width, int height, int[] image,boolean autoRepaint)
	{
		m_Width=width;
		m_Height=height;
		m_Image=image;
		setSize(width,height);
		setPreferredSize(new java.awt.Dimension(m_Width,m_Height));
		m_RepaintOnImageUpdate=autoRepaint;
	}
	
	
	public void SetImage(int[] newImage)
	{
		m_Image=newImage;
		if (m_RepaintOnImageUpdate)
			repaint();
	}
	public void paintComponent(Graphics g)
	{
		super.paintComponent(g);
		m_Counter=m_Counter+1;
		Graphics2D g2=(Graphics2D)g;
		int[] rgb=null;
		for (int i=0;i<m_Width;i++)
			for (int j=0;j<m_Height;j++)
			{
				try
				{
									
					rgb=GetColor(i, j, m_Image, m_Height, m_Width);
				}
				catch (java.lang.NullPointerException e)
				{
					//System.out.println(String.format("(%1$d,%2$d) ",i,j));
				}
				if (rgb!=null)
					g2.setColor(new Color(rgb[0],rgb[1],rgb[2]));
				
				g2.drawLine(i, j, i, j);
			}
	}
	
	public int[] GetColor(int x, int y, int[] rawValues,int height,int width)
	{
		int baseIndex = x*3+y*width*3;
		int green=baseIndex+1;
		int blue=baseIndex+2;
		int red = baseIndex;
		return new int[]{rawValues[red],rawValues[green],rawValues[blue]};
	}
	
}
