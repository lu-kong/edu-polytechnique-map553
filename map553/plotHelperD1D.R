plotHelperD1D <- function (xdomain,xlab,ydomain,ylab,estimatorDerived,main){
  xpoints <- (1:100)/100
  estimatedpoints <- estimatorDerived(xpoints)
  plot(xpoints*(xdomain[2]-xdomain[1])+xdomain[1],estimatedpoints*(ydomain[2]-ydomain[1])/(xdomain[2]-xdomain[1]),xlab=xlab,ylab=ylab,col='red',lwd=2,type="l")
  title(main=main)
}