Jiawei Wu (worked alone)

Eta conversion and beta reduction work fine

alpha renaming basically just adds a 0 whenever its a variable that appears 
in freevar, freevar function was taken from lecture

however, alpha renaming has some bugs which doesn't rename variables at 
every step? I have tried fixing it, but this is my best attempt

attempted to alpha rename all variables before calling beta reduction, 
(basically every variable that is free, change their name), need to take 
care of too many corner cases, so didn't bother on continuing that