# Calc

A basic math expressions parser. I tried to do this project when I started learning Haskell without looking at any standard solution or caring too much about efficiency and I'm trying to improve it now.

It transforms `((1 + x) * (y / 9) + 7) * 2` to:

```
					 *        
					 |        
					 -------- 
					/        \
					+        2
					|         
				  -------    
				 /       \   
				 *       7   
				 |           
			  -----         
			 /     \        
			 +     /        
			 |     |        
			 --    --       
			/  \  /  \      
			1  x  y  9      
```

Underscores (`_`) are reserved.
