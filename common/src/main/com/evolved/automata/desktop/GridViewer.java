package com.evolved.automata.desktop;
import javax.swing.*;




import java.util.*;

public class GridViewer extends JFrame {

	public enum COLOR
	{
		RED, BLUE, BLACK, WHITE,GREEN,YELLOW
	}
	RGBRenderer i_Renderer;
	
	int[] j_RawImage;
	int i_BaseWidth;
	int i_BaseHeight;
	int i_LWidth;
	int i_LHeight;
	int[] i_LogicalColors;
	
	public GridViewer(int baseWidth, int baseHeight, int logicalWidth, int logicalHeight)
	{
		super();
		i_BaseWidth=baseWidth;
		i_BaseHeight=baseHeight;
		i_LWidth=logicalWidth;
		i_LHeight=logicalHeight;
		j_RawImage = new int[baseWidth*baseHeight*3];
		i_LogicalColors=new int[3*logicalWidth*logicalHeight];
		i_Renderer = new RGBRenderer(baseWidth,baseHeight,j_RawImage,true);
		getContentPane().add(i_Renderer);
		setTitle("Grid viewer");
		add(i_Renderer);
		pack();
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}
	
	
	public void UpdateColors(COLOR[][] colors)
	{
		int[] rgb;
		int baseIndex;
		for (int i=0;i<i_LWidth;i++)
		{
			for (int j=0;j<i_LHeight;j++)
			{
				rgb=MapColor(colors[i][j]);
				baseIndex=3*BasicTools.MapCoord(i, j, i_LWidth);
				i_LogicalColors[baseIndex]=rgb[0];
				i_LogicalColors[baseIndex+1]=rgb[1];
				i_LogicalColors[baseIndex+2]=rgb[2];
			}
		}
		BasicTools.MapDiscretizeImage(j_RawImage, i_BaseWidth, i_BaseHeight, i_LogicalColors, i_LWidth, i_LHeight);
		i_Renderer.SetImage(j_RawImage);
	}
	
	private int[] MapColor(COLOR color)
	{
		switch (color)
		{
			case BLACK:
				return new int[]{0,0,0};
			case WHITE:
				return new int[]{127,127,127};
			default:
				return new int[]{0,0,0};
				
		}
	}
	
}
