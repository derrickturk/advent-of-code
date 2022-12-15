BEGIN { FS = "[ =,:]+" }
{ print $4, $6, $12, $14 }
