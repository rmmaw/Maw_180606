#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jun  6 15:34:54 2018

@author: reillymaw
"""

import requests 
    

# grab the contents off of the wepage provided. 
if __name__ == "__main__":
    
    
    def getWebContents(wepage):
        url = requests.get(webpage)
        # Assuming # is not a character 
        raw = (url.text.replace('# ',''))
        text_file = open("Input.txt", "w")
        text_file.write(raw)
        text_file.close()
    
    webpage = "https://raw.githubusercontent.com/ToJen/Quorum-Enterprise-Blockchain/master/README.md"
    getWebContents(webpage)
