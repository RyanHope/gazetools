# Classification ignorning blinks
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, 
                      500, 1680, 1050, 473.76, 296.1, 
                      smi_ezl, smi_exl, smi_eyl))
d.c <- classify.MM(d.pva@v)
str(d.c)
 
# Classification accounting for blinks
d.pva <- with(smi, pva(smi_sxl, smi_syl, 
                      500, 1680, 1050, 473.76, 296.1, 
                      smi_ezl, smi_exl, smi_eyl, pupil=smi_dxl))
d.c <- classify.MM(d.pva@v, blinks=d.pva@blinks)
str(d.c)
