begin

f = addfile("sresa1b_ncar_ccsm3-example.nc", "r")

time_index = 0
lev_index = 2

temp = f->tas(time_index,:,:)
uavg = f->ua(time_index, lev_index, :, :)
pres = f->plev(:)
prec = f->pr(time_index,:,:)

wks = gsn_open_wks("pdf", "testFilePlot.pdf")

res = True
res@cnFillOn = True
res@gsnDraw = False
res@gsnFrame = False

plot = new(3, graphic)

res@tiMainString = "TAS"
plot(0) = gsn_csm_contour_map_ce(wks, temp, res)
res@tiMainString = "UAvg, 850 hPa"
plot(1) = gsn_csm_contour_map_ce(wks, uavg, res)
res@tiMainString = "Precip. flux"
res@cnLinesOn = False
plot(2) = gsn_csm_contour_map_ce(wks, prec, res)

panelres = True
gsn_panel(wks, plot, (/3, 1/), panelres)

end
