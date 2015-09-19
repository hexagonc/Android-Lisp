package com.evolved.automata.desktop;

import java.io.File;
import java.io.FileOutputStream;




public class BasicTools {

	public static final int RED=0;
	public static final int GREEN=1;
	public static final int BLUE=2;
	
	
	public static int MapCoord(int i, int j, int width)
	{
		return i+width*j;
	}
	
	public static int[] SubtractColors(int[] c1, int[] c2)
	{
		int[] nc = new int[3];
		for (int i=0;i<3;i++)
			nc[i]=Math.max(c1[i], c2[i])-Math.min(c1[i], c2[i]);
		return nc;
	}
	
	
	public static int[] GetColorBounds(int[] colors){
		int minRed=0,maxRed=0,minBlue=0,maxBlue=0,minGreen=0,maxGreen=0;
		
		boolean first=true;
		for(int i=0;i<(colors.length/3);i++)
		{
			if (first)
			{
				first=false;
				minRed=maxRed=colors[i*3];
				minGreen=maxGreen=colors[i*3+1];
				minBlue=maxBlue=colors[3*i+2];
				
			}
			else
			{
				if (minRed>colors[i*3])
					minRed=colors[i*3];
				
				if (minGreen>colors[i*3+1])
					minGreen=colors[i*3+1];
				
				if (minBlue>colors[3*i+2])
					minBlue=colors[3*i+2];
				
				if (maxRed<colors[i*3])
					maxRed=colors[i*3];
				
				if (maxGreen<colors[i*3+1])
					maxGreen=colors[i*3+1];
				
				if (maxBlue<colors[3*i+2])
					maxBlue=colors[3*i+2];
			}
		}
		return new int[]{minRed,maxRed,minGreen,maxGreen,minBlue,maxBlue};
	}
	
	public static void FilterBrightnessPercent(int[] colors, int width, int height, double filterpercent,boolean scaleByMaxBrightness)
	{
		if (scaleByMaxBrightness)
		{
			double gray; 
			int[] colorBounds = GetColorBounds(colors);
			int[] grayScaleBounds = GetGrayScaleBounds(colors);
			int maxGray=grayScaleBounds[1];
			
			int[] black = new int[]{0,0,0};
			for (int i=0;i<width;i++)
				for (int j=0;j<height;j++)
				{
					gray=ConvertToGrayscale(GetColor(i, j, colors, height, width));
					if ((1.0*gray)/maxGray<filterpercent)
						SetColor(colors, width, height, i, j, black);
				}
		}
		else
			FilterBrightnessPercent(colors,width,height,filterpercent);
	}
	
	public static void FilterBrightnessPercent(int[] colors, int width, int height, double filterpercent)
	{
		double gray;
		int[] black = new int[]{0,0,0};
		for (int i=0;i<width;i++)
			for (int j=0;j<height;j++)
			{
				gray=ConvertToGrayscale(GetColor(i, j, colors, height, width));
				if (gray/255.0<filterpercent)
					SetColor(colors, width, height, i, j, black);
			}
	}
	
	
	public static int[] ExtractColors(int[] baseData, int width, int height, int botI,int botJ,int topI,int topJ)
	{
		int newWidth = (topI-botI+1);
		int newHeight = (topJ-botJ+1);
		int[] outColors = new int[newWidth*newHeight*3];
		int[] baseColor;
		int outIndex=0, red, green,blue;
		for (int j=botJ;j<=topJ;j++)
			for (int i=botI;i<=topI;i++)
			{
				baseColor=GetColor(i, j, baseData, height, width);
				red=outIndex+RED;
				green= outIndex+GREEN;
				blue = outIndex+BLUE;
				outColors[red]=baseColor[RED];
				outColors[blue]=baseColor[BLUE];
				outColors[green]=baseColor[GREEN];
				outIndex+=3;
			}
		return outColors;
	}
	
	public static void SetColor(int[] data, int width, int height, int x, int y, int[] newColor)
	{
		int baseIndex = 3*(x+y*width);
		for (int i=0;i<3;i++)
			data[baseIndex+i]=newColor[i];
		
	}
	
	public static double Distance(int[] color1, int[] color2)
	{
		double d=0;
		for (int i=1;i<3;i++)
			d+=(color1[i] - color2[i])*(color1[i] - color2[i]);
		d+=2*(color1[0] - color2[0])*(color1[0] - color2[0]);
		return Math.sqrt(d);
	}
	
	public static int[] ImageSubtractDifference(int[] img1, int[] img2, int width, int height)
	{
		int[] output = new int[3*width*height];
		double distance;
		int[] c1, c2;
		//int[] black = new int[]{128,128,128};
		for (int i=0;i<width;i++)
			for (int j=0;j<height;j++)
			{
				c1=GetColor(i, j, img1, height, width);
				c2=GetColor(i, j, img2, height, width);
				
				if (Distance(c1, c2)>0)
					SetColor(output, width, height, i, j, SubtractColors(c1, c2));
				else
					SetColor(output, width, height, i, j, SubtractColors(c1, c2));
			}
		return output;
		
	}
	
	
	public static int[] GetGrayScaleBounds(int[] colors){
		int minGray=0,maxGray=0, red, green,blue;
		int aColor;
		boolean first=true;
		for(int i=0;i<(colors.length/3);i++)
		{
			
			if (first)
			{
				first=false;
				red=3*i+RED;
				blue=3*i+BLUE;
				green=3*i+GREEN;
				minGray=maxGray=ConvertToGrayscale(colors[red], colors[green], colors[blue]);
				
			}
			else
			{
				red=3*i+RED;
				blue=3*i+BLUE;
				green=3*i+GREEN;
				aColor=ConvertToGrayscale(colors[red], colors[green], colors[blue]);
				if (aColor>maxGray)
					maxGray=aColor;
				if (aColor<minGray)
					minGray=aColor;
				
			}
		}
		return new int[]{minGray,maxGray};
	}
	
	public static int ConvertToGrayscale(int red, int green, int blue)
	{
		
		return (red+blue+green)/3;
	}
	
	public static int ConvertToGrayscale(int[] colors)
	{
		
		return (colors[RED]+colors[GREEN]+colors[BLUE])/3;
	}
	
	
	public static int[] GetColor(int x, int y, int[] rawValues,int height,int width)
	{
		int baseIndex = x*3+y*width*3;
		int green=baseIndex;
		int blue=baseIndex+1;
		int red = baseIndex+2;
		return new int[]{rawValues[red],rawValues[blue],rawValues[green]};
	}
	
	  public static boolean SavePPM(String filename, int buffer[], int width, int height)
	  {
		  if (buffer==null)
			  return false;
		  
		  byte[] inputBuffer = new byte[3*height*width];
		  for (int i=0;i<inputBuffer.length;i++)
			  inputBuffer[i]=(byte)(buffer[i] & 0xFF);
	    try
	    {
	      FileOutputStream fos = new FileOutputStream(new File(filename));
	      String header = "P6\n"+width+" "+height+"\n255\n";
	      fos.write(header.getBytes(), 0, header.length());
	      //fos.close();
	      fos.write(inputBuffer, 0, width*height*3);
	      fos.close();
	    }
	    catch (Exception e)
	    {
	      return false;
	    };

	    return true;
	  }
	  
	
	
	
	public static void MapDiscretizeImage(int baseImage[], int outerWidth, int outerHeight, int[] logicalColors, int logicalWidth, int logicalHeight) 
	{
		int stepI = outerWidth/logicalWidth,stepJ = outerHeight/logicalHeight;
		int partialStepI = outerWidth % logicalWidth, partialStepJ=outerHeight % logicalHeight;
		
		if ((stepI*logicalWidth+partialStepI)!=outerWidth)
			System.out.println("Assert failure");
		
		
		int botI,botJ,topI,topJ;
		int[] newColor;

		
		
		int counter=0;
		for (int j=0;j<logicalHeight;j++)
			for (int i=0;i<logicalWidth;i++)
			{
				counter++;
				botI= i*stepI;
				botJ=j*stepJ;
				if (i!=(logicalWidth-1))
					topI= (i+1)*stepI;
				else
					topI=(i+1)*stepI-1+partialStepI;
				if (j!=(logicalHeight-1))
					topJ= (j+1)*stepJ;
				else
					topJ= (j+1)*stepJ-1+partialStepJ;
					
				newColor=GetColor(i, j, logicalColors, logicalHeight, logicalWidth);
				Fill(baseImage,newColor,outerWidth,botI,botJ,topI,topJ);
			}
		

	}
	
	public static void Fill(int[] basePixels,int[] color, int Width, int botI,int botJ,int topI,int topJ )
	{
		int baseIndex;
		int green;
		int blue;
		int red;
		for (int j=botJ;j<=topJ;j++)
			for (int i=botI;i<=topI;i++)
			{
				baseIndex = i*3+j*Width*3;
				green=baseIndex+GREEN;
				blue=baseIndex+BLUE;
				red = baseIndex+RED;
				
				try
				{
					basePixels[red]=color[RED];
					basePixels[green]=color[GREEN];
					basePixels[blue]=color[BLUE];
				}
				catch (Exception e)
				{
					//System.out.println("error");
				}
			}
	}
	
}
