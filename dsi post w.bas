'
'  give this thing a .bmp !
'
var filename = ".bmp"


/' -- dot stacked image (lossy compression format) - 2022 Mar 11 - by dafhi

  preview build - saved files compatibility not guaranteed.
 
  New architecture: chunks.  n-bit header + vari_bits_per_dot * cdots.
  
  benefits: streaming, better quality, experimentation platform
  
  note:  some cpu architectures run about 75% slower.  The bottleneck appears to
  happen in sRGBi.Cast.
 
  i'm still figuring out my coding style.  huge apologies ;)
   
   ---------
    updates
   ---------
  
  March cumulative -
  
  March 11 - "official" detection
  March 10 - experiment:
    combined flat & gradient dot styles into one encode
  2 different encode styles.  one might be fast, other might look artsy.
  
  March 8 - eigens order (sub _props_from), file Header udt precedes version-specific code
  Mar. 7 - dot: flat vs gradient
  sRGBi.dcol_sq -> delta_col (now uses sqr)
  sRGBi.cast and delta_col to ulong fixes
  eigens ordering in dsi_imager.props_from
  2. hash more responsive to small seed
  1. improved eigenvectors foundation
 
'/


'#include "inc/dsi version.bas"
/' -- "dsi version.bas" - 2021 Jan 16 - by dafhi

  "dot stacked image" lossy format.  version-specific codez

  - process overview
 
  1. anti-aliased dot with properties  x y sRGBA rad slope
  2. hash function initialized w/ array indx as seed, then run once
  3. each property runs the hash.  first prop i use seed
  4. to converge the image, i loop through seeds
    A. take a before and after measurement of dot vs. image (under same dot area)
    B. save seed most-ish improving the render
 
  ==================================================
   
  ---------------
    inspiration
  ---------------
 
  The Demoscene
  Commodore Amiga
  ray (path) tracing community
  inventor Veljko Milkovic
  Advanced Micro Devices
  the free energy community
   
  ------------------
    acknowledgement
  ------------------
 
  fxm:  documentation, forum help, knowledge
  dodicat, Paul Doe, Stonemonkey:  FB friends
  badidea, dodicat, D.J. Peters:  early project comments
   
     -----------------
       dedication   
   -----------------
   
  United States welfare system
  FreeBASIC community

'/

' ------ dsi boilerplate.bas ---------------
'
#define def   #define

'' replaces int()
def flo(x) (((x)*2.0-0.5)shr 1) '' http://www.freebasic.net/forum/viewtopic.php?p=118633
def ceil(x) (-((-(x)*2.0-0.5)shr 1))

#undef int
def int     as Integer
def sng     as single
def dbl     as double
def bool    as boolean

def decl    declare
def virt    virtual
def func    function
def prop    property
def oper    operator
def csr     constructor
def ret     return
def float   single
def ac      as const


def min( a, b)        iif( (a)<(b), (a), (b) )
def max( a, b)        iif( (a)>(b), (a), (b) )

function clamp( in sng, hi sng = 1, lo sng = 0) sng
  return in + (in - hi) * (in > hi) + (in - lo) * (in < lo)
End Function

' --- (1 byte less in fileheader yay)
type RGB24 field = 1
  as ubyte          r,g,b
  decl oper         cast ac ulong
  decl oper         cast ac string '' 2021 Dec 30:  added
end type

oper RGB24.cast ac ulong
  return rgb(r,g,b)
end oper

oper RGB24.cast ac string
  return str(r) + " " + str(g) + " " + str(b)
end oper

 
  namespace ns_fileheader '' 2022 March 8

type dsiHeader field = 1    ' byte align
  as string*38    mesg
end type

type infoHeader field = 1 '' byte align
  decl prop       w int
  decl prop       h int
  decl prop       w( int)
  decl prop       h( int)
 
  as RGB24        avgcol
  as ushort       wm, hm
end type

prop infoHeader.w( param int)
  wm = clamp(param, 65536, 1) - 1
end prop

prop infoHeader.h( param int)
  hm = clamp(param, 65536, 1) - 1
end prop

prop infoHeader.w int
  return wm + 1
end prop

prop infoHeader.h int
  return hm + 1
end prop

function size int
  return _
    sizeof(dsiHeader) + _
    len(infoHeader)
end function

oper <>(l as dsiHeader, r as dsiHeader) int
 
  dim int _err
  dim as byte ptr a = @l.mesg[0] '' 2021 Dec 31
  dim as byte ptr b = @r.mesg[0]
 
  for i int = 0 to len(dsiHeader)-1
    _err += a[i] <> b[i]
  next
  return _err <> 0
 
end oper

end namespace ' --- ns_fileHeader

sub adjust_filename_dsi( byref filename as string )
  filename = left(filename, len(filename) - 4) + ".dsi"
end sub

sub _copy_bytes(des() as byte, src() as byte, u int, l int)
  for i int = 0 to ubound(des)
    des(i)=src(i)
  next
end sub


#undef rnd '' user RNG for procedural debug / testing

function rnd ac single:  static as ulong a=1, b
  a *= a
  a xor= b
  b += 1
  return a / (culngint(1) shl 32)
end function


function round( in ac double) as string
  return str( flo( in * 8 + .5 ) \ 8)
end function

union suspicion_suppressor   ' suspicious ptr tango
  as any ptr          a
  as ubyte ptr        b
  as ushort ptr       sho
  as ulong ptr        l
  as ulongint ptr     li
  As Single Ptr       s
  as double ptr       d
End Union


dim shared as suspicion_suppressor  gp

 
type Rect1 '' used by aadot_nosq.draw but placed here for debug
  int                 X0, Y0, X1, Y1
  decl oper           cast as string
End Type

oper Rect1.cast as string
  return str(x0) & " " & str(y0) & " " &_
    str(x1) & " " & str(y1) & " "
end oper


dim shared as Rect1          rc

const c13 = chr(10)+chr(13)


const tau = 8 * atn(1)
const pi = 4 * atn(1)


' -------- debugger -------------------
'
function strBin(p as any ptr, cBytes as longint = 1) as string
  var s = ""
  gp.a = p + cbytes - 1
  for j as longint = 1 to cBytes
    for i as long = 7 to 0 step -1
      s += str((*gp.b shr i) and 1)
    next:  gp.a -= 1
  next
  return s
end function

sub print_bytes(pp as ubyte ptr, u int)
  for p as ubyte ptr = pp to @pp[u]
    ? *p; " ";
  next 
End Sub


'def dbg_subs

'' follow a print statement w/ rand value to check section execution
def qc  ; rnd; " "; klkl
def qq  sbuf_show:? stream.bitpos
dim shared int gmode
'
 ' --------- debugger -------------

'
' -------- dsi boilerplate.bas


' --- dsi imvars.bas
'
type imagevars '2021 Dec 19 - by dafhi
 
  decl                  csr( as any ptr = 0) '' constructor
  declare               destructor
  decl oper             cast as any ptr
  declare sub           get_info( as any ptr = 0 )    '' example:  get_info imagecreate(400, 300)
  declare sub           create(w int=0, h int=0, col as ulong=&HFF000000)
  declare sub           bmp_load( ByRef filename As String )
  declare sub           blit(byref pdest as imagevars ptr=0, x int=0, y int=0, size as ubyte=0)
 
  int                   w,h,bpp,bypp,pitch,rate '' fb standard
  as any ptr            im, pixels
  as string             driver_name
 
  sng                   wh, hh, diagonal        '' specialized
  int                   wm, hm, u, pitchBy
  as ulong ptr          p32
  decl sub              release
    private:
  decl sub              _specialized
end type

csr imagevars( im as any ptr):  if imageinfo(im) then exit csr
  get_info im
end csr

Destructor.imagevars:  release
End Destructor

sub imagevars.release
  If ImageInfo(im) = 0 Then ImageDestroy im:  im=0
End Sub

oper imagevars.cast as any ptr
  return im
end oper

sub imagevars._specialized
  wm = w - 1:  wh = w/2
  hm = h - 1:  hh = h/2
  pitchBy = pitch \ bypp:  u = h*pitchBy - 1
  p32 = pixels:  diagonal = sqr(w*w + h*h)
End Sub

sub imagevars.get_info( _im as any ptr )
  if _im = 0 then ScreenInfo w,h, bpp, bypp, pitch, rate, driver_name:  pixels = screenptr:  im = 0:  _specialized:  exit sub
  if Imageinfo(_im) = 0 then im = _im: ImageInfo im, w, h, bypp, pitch, pixels:  bpp = bypp * 8:  _specialized
end sub

sub imagevars.create(_w int, _h int, col as ulong)
  _w = abs(_w) '' 2021 Dec 19
  _h = abs(_h)
  if _w<1 or _h<1 then exit sub
  release:  get_info imagecreate(_w,_h,col)
End Sub

sub imagevars.blit(byref pdest as imagevars ptr, x int, y int, size as ubyte) '2017 Aug 31
  if size>1 then
    var sizem=size-1:  dim as imagevars  dest
    if pdest=0 then dest.get_info: pdest=@dest
    var x1=x+wm*size: if x1>pdest->wm then x1=pdest->wm
    var y1=y+hm*size: if y1>pdest->hm then y1=pdest->hm
   
    for iy as long=y to y1 step size
      dim as ulong ptr psrc = p32 + ((iy-y)\size) * pitchBy
      if pdest=0 or pdest->im=0 then
        for ix as long=x to x1 step size
          line (ix,iy)-(ix+sizem,iy+sizem),psrc[(ix-x)\size],bf:  next
      else
        for ix as long=x to x1 step size
          line pdest->im,(ix,iy)-(ix+sizem,iy+sizem),psrc[(ix-x)\size],bf:  next
      endif
    next
    
  else
    
    if pdest=0 then:  put (x,y), im, pset
    else:             put pdest->im, (x,y), im, pset
    endif:  endif
End Sub

sub imagevars.bmp_load( filename As String )  'modified official fb sample
  Dim As Long filenum = FreeFile()
  dim as long _w, _h '' 2021 Dec 19
  for i int = 1 to 2
    If Open( filename For Binary Access Read As #filenum ) = 0 Then
      Get #filenum, 19, _w
      Get #filenum, 23, _h
      close #filenum
      create _w, _h
      bload filename, im
      exit for
    endif
    Close #filenum
    filename = exepath & "\" & filename
  next
End sub

' ----------------------------------- more boiler..

type v3
  sng         x, y, z
  decl        csr
  decl        csr( as ulong)
  decl        csr( as v3)
  decl        csr( as RGB24)
  decl        csr( sng, sng, sng)
  decl oper   let( as ulong)
  decl oper   cast as ulong
  decl oper   cast as string
End Type

csr v3
end csr

csr v3( rr sng, gg sng, bb sng):  x = rr: y = gg: z = bb
end csr

csr v3( col as v3)
  this = col
end csr

csr v3( col as RGB24)
  this = type(col.r,col.g,col.b)
end csr

csr v3( col as ulong)
  this = col
end csr

oper v3.let( col as ulong)
  x = (col shr 16)and 255
  y = (col shr 8)and 255
  z = col and 255
end oper

oper v3.cast as ulong
  return rgb(x, y, z)
end oper

oper v3.cast as string
  return str(x) & " " & str(y) & " " & str(z)
end oper


Type sRGBi    '' v3 + iteration component
  as v3           sum
  sng             iter
 
  decl            csr '' constructor
 
  decl            csr( sng=0, sng=0, sng=0, sng=0)
  decl            csr( as v3, sng=1)
  decl            csr( ac ulong, sng=1)
 
  decl oper       cast ac v3        '' as const
  decl oper       Cast ac ULong       
  decl oper       cast ac string
 
  decl prop       delta_col( as ulong) as ulong
End Type

csr sRGBi
end csr

csr sRGBi( rr sng, gg sng, bb sng, i sng)
  sum.x = rr: sum.y = gg: sum.z = bb: iter = i
end csr

csr sRGBi( s ac ulong, i sng)
  this = type( type<v3>(s), i )
end csr

csr sRGBi( s as v3, i sng):  sum = s: iter = i
end csr

operator sRGBi.Cast ac v3               '' as const
  return sum / iter
end operator

operator sRGBi.Cast ac ulong
  Static sng _mul:  _mul = 1 / iter
    
    Return rgb( _
  clamp( sum.x * _mul, 255.999) -.5, _ '' 2022 Mar 3
  clamp( sum.y * _mul, 255.999) -.5, _
  clamp( sum.z * _mul, 255.999) -.5 )
End oper

operator sRGBi.Cast ac string
  return str(sum.x) + " " + str(sum.y) + " " + str(sum.z) + " " + str(iter)
end operator

prop sRGBi.delta_col(c0 as ulong) as ulong
  dim sng smul = 1 / iter
  dim as long         a=(c0 and 255) - (clamp( sum.z * smul, 255.999) -.5) '' 2022 Mar 3
  dim as long b=a*a:  a=((c0 shr 8)and 255) - (clamp( sum.y * smul, 255.999) -.5)
  b += a*a:             a=((c0 shr 16)and 255) - (clamp( sum.x * smul, 255.999) -.5)
  return sqr(b+a*a) '' 2022 Mar 4 [added sqr]
end prop


type im_downscaler extends imagevars
                        '2017 Aug 17
  declare sub         downscale(byref as imagevars ptr=0, sng=0, sng=0, sng=0, sng=0)
  decl sub            downscale_from(byref as im_downscaler ptr, sng = .5)
  decl prop           avg_col as RGB24
 
 private:

  declare sub         aascan(yDes as long, alp sng)
  as imagevars ptr    pdes            ' aablit
  as long             yDes1D, ySrc1D  '
  sng                 sx, x_step      '
  sng                 sy, y_step      '
  as sRGBi            a(any)
End Type

sub im_downscaler.downscale_from( byref src as im_downscaler ptr, scale sng)
  create _
  src->w*scale, src->h*scale
  src->downscale @this
end sub

sub im_downscaler.aascan(yDes as long, alp sng)
  if yDes < 0 or yDes > pdes->hm then exit sub
  yDes1D = yDes * pdes->pitchBy
  #Macro SngAry()
    a(i).sum.x += al*((src and &HFF0000)shr 16)
    a(i).sum.y += al*((src and &HFF00)shr 8)
    a(i).sum.z += al*(src and &HFF)
  #endmacro
  dim sng al
  for xSrc int = 0 to wm
    var xDesL = flo(sx):  sx += x_step
    var xDesR = flo(sx)
    var i = yDes1D + xDesL
    dim as ulong src = p32[ySrc1D + xSrc]
    if xDesL < xDesR then
      if xDesL >= 0 and xDesL < pdes->w then
        al = (xDesR - (sx-x_step)) * alp
        a(i).iter += al
        SngAry()
      endif
      if xDesR >= 0 and xDesR < pdes->w then
        al = (sx - xDesR) * alp
        i += 1:  a(i).iter += al
        SngAry()
      endif
    elseif xDesL >= 0 and xDesL < pdes->w then
      al = x_step * alp
      a(i).iter += al
      SngAry()
    endif
  next
end sub

sub im_downscaler.downscale(byref dest as imagevars ptr, _w sng, _h sng, x sng, y sng)'2017 Aug 17
  static as im_downscaler  scr
 
  if dest = 0 then
    pdes = @scr
    scr.get_info
  else
    pdes = dest
  endif
 
  redim a(pdes->h * pdes->pitchBy - 1)

  if _w=0 then _w=pdes->w
  if _h=0 then _h=pdes->h
 
  x_step = _w / w
  y_step = _h / h
 
  for ySrc as long = 0 to hm
    sy=y
    var yDesT = flo(y):  y += y_step
    var yDesB = flo(y):  sx = x
    ySrc1D = ySrc * pitchBy
    if yDesT < yDesB then
      aascan yDesT, yDesB - (y-y_step):  sx = x
      aascan yDesB, y - yDesB
    else 'equal
      aascan yDesT, y_step
    endif
  next:  y -= y_step * h
 
  var x0 = iif( x < 0, 0, flo(x))
  var x1 = flo(x + _w): if x1 > pdes->wm then x1 = pdes->wm
  var y0 = iif( y < 0, 0, flo(y))
  var y1 = flo(y + _h): if y1 > pdes->hm then y1 = pdes->hm
 
  for y int = y0 to y1
    var ipitch = y*pdes->pitchBy
    for i int = ipitch + x0 to ipitch + x1
      pdes->p32[i] = a(i)
    next
  next
  erase a
end sub

prop im_downscaler.avg_col as RGB24
  var r = 0ul, g = 0ul, b = 0ul
  var c = w * h
  for y int = 0 to hm*pitchBy step pitchBy
    for i int = y to y + wm
      r += (p32[i] shr 16) and &HFF
      g += (p32[i] shr 8) and &HFF
      b += p32[i] and &HFF
    next:  next
  return type( r/c, g/c, b/c )
end prop
 


operator -( l ac v3, r ac v3) as v3:  return type( l.x-r.x, l.y-r.y, l.z-r.z):  end oper
operator +( l ac v3, r ac v3) as v3:  return type( l.x+r.x, l.y+r.y, l.z+r.z):  end oper
operator *( l ac v3, r ac single) as v3:  return type<v3>( r*l.x, r*l.y, r*l.z):  end oper
operator *( l ac single, r ac v3) as v3:  return type<v3>( l*r.x, l*r.y, l*r.z):  end oper
operator /( l ac v3, r sng) as v3:  r=1/r: return type( r*l.x, r*l.y, r*l.z):  end oper
operator -( l ac v3) ac v3:  return type( -l.x, -l.y, -l.z):  end oper


oper +( l ac sRGBi, r ac sRGBi) ac sRGBi:  return type<sRGBi>( l.sum+r.sum,  l.iter+r.iter):  end oper
oper -( l ac sRGBi, r ac sRGBi) ac sRGBi:  return type<sRGBi>( l.sum-r.sum,  l.iter-r.iter):  end oper


enum fill_mode
  solid
  image
  add
end enum


type sRGBi_buf
  decl prop         w int
  decl prop         h int
  decl prop         wm int
  decl prop         hm int
 
  decl sub          resize( int, int)
  decl sub          fill( byval as v3 = type(0,0,0), sng = 1, int = 0,  byref as imagevars ptr = 0)
  decl sub          view( int=0, int=0, as any ptr = 0)
 
  as sRGBi          mydata(any, any) '' 2022 Jan 2 [old: data(any, any)
 
 private:
  decl sub          clip_calc( as imagevars ptr, int, int, byref as Rect1 ptr)
  int               clip_x0
  int               clip_y0
End Type

prop sRGBi_buf.w int:  return ubound(mydata,1)+1
end prop
prop sRGBi_buf.h int:  return ubound(mydata,2)+1
end prop
prop sRGBi_buf.wm int:  return ubound(mydata,1)
end prop
prop sRGBi_buf.hm int:  return ubound(mydata,2)
end prop

sub sRGBi_buf.resize( ww int, hh int)
  redim mydata( ww-1, hh-1)
End Sub

sub sRGBi_buf.clip_calc( p as imagevars ptr, x int, y int, byref prc as Rect1 ptr)
  clip_x0 = -x * (x<0): prc->x0 = x + clip_x0
  clip_y0 = -y * (y<0): prc->y0 = y + clip_y0
  prc->x1 = x + wm: prc->x1 += (prc->x1 - p->wm) * (prc->x1 > p->wm)
  prc->y1 = y + hm: prc->y1 += (prc->y1 - p->hm) * (prc->y1 > p->hm)
end sub

sub sRGBi_buf.fill( byval _col as v3, stren sng, mode int,  byref p_imvsrc as imagevars ptr )
 
  if hm < 1 then exit sub '' 2022 Mar 4
  
  select case ac mode '' as const
 
  case fill_mode.solid
    var col = type<sRGBi>(_col * stren, stren)
    
    for p as sRGBi ptr = @mydata(0,0) to @mydata(wm, hm)
      *p = col
    next
 
  case fill_mode.add
    for p as sRGBi ptr = @mydata(0,0) to @mydata(wm, hm)
      p->sum += _col
    next
 
  case fill_mode.image
    static as Rect1 rc_src
   
    clip_calc p_imvsrc, 0, 0, @rc_src
   
    for y int = rc_src.y0 to rc_src.y1
      var p32 = p_imvsrc->p32 + y * p_imvsrc->pitchBy
     
      for x int = rc_src.x0 to rc_src.x1
        mydata(x, y).sum = type<v3>( p32[x] ) * stren
        mydata(x, y).iter = stren
        Next:  Next
       
  end select
end sub

sub sRGBi_buf.view( x int, y int, im as any ptr)
  static as Rect1 rc_des
  static as imagevars des
 
  des.get_info im
  clip_calc @des, x, y, @rc_des

  for y = rc_des.y0 to rc_des.y1
    for x = rc_des.x0 to rc_des.x1
      des.p32[y * des.pitchBy + x] = mydata(x + clip_x0 - rc_des.x0, y + clip_y0 - rc_des.y0)
        next: next
       
end sub


'' helper
dim shared as ubyte                    magnification = 1

type proc_view
  as im_downscaler  src
  as sRGBi_buf      sbuf
  decl sub          get_info( as any ptr = 0)
  decl prop         w ac integer
  decl prop         h ac integer
  decl prop         cPels ac ulongint
  decl prop         get_err as double
  decl sub          work_scale( as im_downscaler ptr, sng)
End Type

prop proc_view.cPels ac ulongint
  return w * h
end prop

prop proc_view.w ac integer
  return src.w
end prop

prop proc_view.h ac integer
  return src.h
end prop

sub proc_view.get_info( im as any ptr)
  src.get_info im
  sbuf.resize w, h
End Sub

prop proc_view.get_err as double
  dim as double sum
  for y int = 0 to src.hm
    for x int = 0 to src.wm
      sum += sbuf.mydata(x,y).delta_col( src.p32[x + y*src.pitchBy] )
    next: next:  return sum / cpels
end prop

sub proc_view.work_scale( psrc as im_downscaler ptr, scale sng)
  src.downscale_from psrc, scale
  sbuf.resize src.w, src.h
End Sub


function hsv(h sng, s sng, v sng) as v3
   h -= 6*flo(h/6)
   var x = clamp(2 - h - 2*(h-3)*(h>3))
   var y = clamp(h +     2*(h-2)*(h>2))
   var z = clamp(h - 2 + 2*(h-4)*(h>4))
   var lo=@x, mi=@y, hi=@z
   if *lo > *hi then swap lo, hi
   if *lo > *mi then swap lo, mi
   if *mi > *hi then swap mi, hi
   s = clamp(s)
   v = clamp(v)
   *lo = v * (*hi - s * (*hi - *lo))
   *mi = v * (*hi - s * (*hi - *mi))
   *hi *= v
   return type( x, y, z )
End Function
'
' ------ dsi imagevars.bas


 
  namespace dsi_hash '' RNG

  /' -- changing the hash will break compatability.
  
  '/

type statelit as ulong

dim as statelit a, b

dim as ulongint mulC = &b10000000001000000001000000010000001000001000010001001011

const lenx8 = len(statelit)*8

sub rotr(byref q as statelit, amount as byte)
  q = (q shl (lenx8 - amount)) or (q shr amount)
End Sub

sub reset( val_a as statelit = 0, val_b as statelit = 0)
  a = val_a
  b = val_b
end sub

function valu( i as ulongint = 0) as statelit '' integer
  
 '' 2022 Mar 2
 
  b += ((a+i) * mulc) shr 1
  rotr b, 16
  a xor= 1 + b
  
  return a
End function

function d( i as ulongint = 0) ac double      '' as const
  return valu(i) / cast(statelit, -1)
end function

function ini( i as ulongint = 0, val_a as statelit = 0, val_b as statelit = 0) as statelit
  reset val_a, val_b
  return valu(i)
End function

End Namespace ' -- dsi_hash --

  
  namespace ns_ECS '' entity component system

type nBitHashFloat
  decl csr (sng = 1, sng = 0)
  decl oper       cast ac single  '' ac my shorthand for 'as const'
  decl oper       cast ac string
  decl oper       let( ac double)

  decl sub        run(as dsi_hash.statelit = 0)
  sng             _sval
  sng             _sngmax
  sng             _sngmin
end type

csr nBitHashFloat( sngmax sng, sngmin sng)
  _sngmin = sngmin
  _sngmax = sngmax
end csr

sub nBitHashFloat.run(seed as dsi_hash.statelit) '' 2022 Mar 2 - renamed _add to seed
  _sval = _sngmin + dsi_hash.d(seed) * (_sngmax - _sngmin) '' hash range 0.0 .. 1.0 inclusive
end sub

oper nBitHashFloat.cast ac single
  return _sval
end oper

oper nBitHashFloat.cast ac string:  static sng q
  q = this:  return str(q)
end oper

oper nBitHashFloat.let( d ac double)
  _sval = d
end oper


type t
 
  decl oper           cast ac string     '' debug info
 
  decl sub            assign_cbits( as ubyte)
 
  decl sub            ini_( int = 0, as dsi_hash.statelit = 0, as dsi_hash.statelit = 0)

  as nBitHashFloat    style = type(1, 0)
  as nBitHashFloat    slope = type(1, .5)
  as nBitHashFloat    rad = type(1, 0)
  as nBitHashFloat    x = type(1, 0)
  as nBitHashFloat    y = type(1, 0)
  as nBitHashFloat    r = type(6, 0) '' hue sat val :p
  as nBitHashFloat    g = type(1, 0)
  as nBitHashFloat    b = type(1, 0)
  'as nBitHashFloat    a = type(1.4, 0.7) '' 
  as nBitHashFloat    a = type(.8, 0.15) '' 
 
  decl prop           full ac ulong
 
  dbl                 bitpos_s, bitpos_delta
  int                 maxinc_frame
 
End Type

sub t.ini_( idx int = 0, seeda as dsi_hash.statelit = 0, seedb as dsi_hash.statelit = 0)
  dsi_hash.ini idx, seeda, seedb
end sub

oper t.cast ac string:  static as string s
  s = "xy " + str(x) + " " + str(y) + " " + c13 + _
   "rad "+ str(rad) + " " + c13 + _
   "r "+ str(r) + " " + c13 + _
   "g "+ str(g) + " " + c13 + _
   "b "+ str(b) + " " + c13 + _
   "a "+ str(a) + " " + c13 + _
   "sl "+ str(slope) + " "
   return s
end oper
 
end namespace


 /' -- anti-aliased rendering primitive -- '/

  namespace AaDot_noSq

dim as sRGBi_buf ptr  pTarget
dim sng               clip_sx1, clip_sy1

sub render_target( byref pTarg as sRGBi_buf ptr)
  pTarget = pTarg
end sub

dim sng cone_h, cone_sq, dxLeft, loptop, loptopSq

sub xy01_( byref pdv as ns_ecs.t ptr)
  clip_sx1 = pTarget->w - .5001 '' moved from render_target
  clip_sy1 = pTarget->h - .5001
  with rc
    .x0 = (pdv->x - pdv->rad):  .X0 = flo(.x0 + .x0 * (.x0 < 0))  '' nicer on cpu than [if x < 0 then x = 0]
    .y0 = (pdv->y - pdv->rad):  .Y0 = flo(.y0 + .y0 * (.y0 < 0))
    .x1 = (pdv->x + pdv->rad):  .X1 = flo(.x1 + (.x1 - clip_sx1) * (.x1 > clip_sx1))
    .y1 = (pdv->y + pdv->rad):  .Y1 = flo(.y1 + (.y1 - clip_sy1) * (.y1 > clip_sy1))
    cone_h = pdv->slope * (pdv->rad + .25)  ''
    cone_Sq = cone_h * cone_h
    loptop = cone_h - 1
    loptopSq = loptop * loptop
    dxLeft = (.X0 + .0 - pdv->x) * pdv->slope
  end with
End Sub

dim sng dx, dy, coneSq_minus_dySq

function circ_err( byref pv as proc_view ptr, byref p as ns_ecs.t ptr ) dbl
  dim dbl sum
  dy = (rc.Y0 + .0 - p->y) * p->slope
  var cPels = 0
  for y int = rc.Y0 to rc.Y1
    dx = dxleft
    coneSq_minus_dySq = cone_Sq - dy*dy
    for x int = rc.X0 to rc.X1
      if dx * dx < coneSq_minus_dySq then
        sum += pv->sbuf.mydata(x,y).delta_col( pv->src.p32[x + y*pv->src.pitchBy] )
        cpels += 1
      endif
      dx += p->slope:  next
    dy += p->slope:  next
  'return (sum+.5) / (cPels+.5) ^ .1
  return sum
end function

'dim int b_dotstyle
enum dot_style
  gradient
  flat
End Enum

  

sub _render( byref p as ns_ecs.t ptr, byval col as sRGBi)
 
  '' xy01_ meant to be called prior
 
  dy = (rc.Y0 + .0 - p->y) * p->slope
 
  static sng reciprocal_alpha: reciprocal_alpha = 1 / (cone_Sq - loptopSq)
  static sng a

  col.sum *= col.iter
  dim int j = p->style
  '? j: sleep 500
  select case ac j '' ac = as const
  'select case ac b_dotstyle '' ac = as const
  
  case dot_style.gradient
    for y int = rc.Y0 to rc.Y1
      dx = dxleft
      coneSq_minus_dySq = cone_Sq - dy*dy
      for x int = rc.X0 to rc.X1
        a = dx * dx
        a = (coneSq_minus_dySq - a) * reciprocal_alpha * -(a < coneSq_minus_dySq)
        pTarget->mydata(x, y).iter += col.iter * a  '' adjusting properties seperately ..
        pTarget->mydata(x, y).sum += col.sum * a    '' .. my OOP not working as expected
        dx += p->slope:  next
      dy += p->slope:  next
  
  case dot_style.flat
  
    for y int = rc.Y0 to rc.Y1
      dx = dxleft
      coneSq_minus_dySq = cone_Sq - dy*dy
      for x int = rc.X0 to rc.X1
        if dx * dx < coneSq_minus_dySq then
          ptarget->mydata(x, y) += col
        endif
        dx += p->slope:  next
      dy += p->slope:  next
  end select
 
end sub

sub draw( x as single, y as single, col as sRGBi, rad sng = .7071, slope sng = 1)
  static as ns_ecs.t   dv
  dv.x = x
  dv.y = y
  dv.rad = rad
  dv.slope = slope
  xy01_ @dv
  _render @dv, col
end sub

end namespace



'dim shared as ns_fileheader.verHeader  verheader
dim shared as ns_fileheader.infoHeader infoHeader


  namespace stream

dim as long     bitpos, pos_oob
dim as ubyte    bytes()

dim int         b_offset
dim as ulong    mask

sub _stream_common(cbits as byte, bitpos_inc int)
  var bypos = bitpos \ 8 '' integer divide
  b_offset = bitpos - bypos * 8
  gp.a = @bytes(bypos)
  mask = (2 ^ cbits - 1)
  bitpos += bitpos_inc
end sub

sub write(valu as short, cbits as byte, bitpos_inc as short)
  _stream_common cbits, bitpos_inc
  *gp.l and= -1 xor (mask shl b_offset) '' 4 byte int ptr
  *gp.l or= (valu and mask) shl b_offset
end sub

function read( cbits as byte, bitpos_inc as short) as short
  _stream_common cbits, bitpos_inc
  return (*gp.l shr b_offset) and mask
end function
  
  dim int flag_official   '' official version flag in saved image
  dim int c_flagbits = 1  '' currently only 1 flag

sub flags_write
  var pos_advance = c_flagbits
  write( flag_official, c_flagbits, pos_advance )
end sub

sub flags_read
  var pos_advance = c_flagbits
  flag_official = read( c_flagbits, pos_advance )
end sub

end namespace ' --- stream



type t_hyper_parameters field = 1 '' byte align
  sng       radScale0
  sng       radDecay
  sng       radDetailRush
  sng       expon_dotsper
  as byte   base_dotsper
  as byte   hdr_cbits
  as byte   hdr_fcbits_base
  as byte   hdr_fcbits_extend
  as string * len(ns_fileheader.dsiHeader)  verHeader
End Type

dim shared as t_hyper_parameters _
  g_hypers_experim

oper = (l as t_hyper_parameters, r as t_hyper_parameters) int
  dim as any ptr pal = @l, par = @r
  dim as byte ptr pbl = pal, pbr = par
  static int diff
  for i int = 0 to len(t_hyper_parameters)-1
    diff += abs(pbl[i] - pbr[i])
  next  
  return diff = 0
end oper


  namespace seed_hdr
  
dim as short    _max

function f_cseedbits( byref p_hypers as t_hyper_parameters ptr, val as byte) as byte
  return p_hypers->hdr_fcbits_base - p_hypers->hdr_fcbits_extend * (val and _max)
end function

function f_possubexit( byref p_hypers as t_hyper_parameters ptr, bits_enc as ulong) int
  swap bits_enc, stream.bitpos
  
  var pos_increment = 0
  var stream_read = stream.read( p_hypers->hdr_cbits, pos_increment )
  var c_seed = f_cseedbits( p_hypers, stream_read )
  swap bits_enc, stream.bitpos
  
  return stream.pos_oob + 1 - p_hypers->hdr_cbits - c_seed '' love off-by-1's  >.<
End Function

sub setmax( hdr_cbits int)
  _max = 2 ^ hdr_cbits - 1
End Sub

end namespace
  

type hdr_vars
  decl csr      (byref as t_hyper_parameters ptr = 0)
  as byte       dot_seed
  int           idot
  int           bits_encoded
  int           dots_encoded
  dbl           bene
  
  as t_hyper_parameters ptr _
    p_hypers
end type

csr hdr_vars(byref p as t_hyper_parameters ptr)
  p_hypers = p
end csr


  
  namespace ns_compat
    
/' -- "official" detection --
 
    intent:
  1. groups hyperparameters for easy experimentation.
  2. coder forgetfulness warning & write prevention
  
  if comparison subs (work_branch & master) don't match,
  "official" flag will be set to 0.

'/

  function work_branch as t_hyper_parameters ptr
    static as t_hyper_parameters hypers

/' -- sub for experimentation '/
  
  hypers.radScale0 = .14
  hypers.radDecay = .48
  hypers.radDetailRush = .15

  hypers.hdr_cbits = 1
  hypers.hdr_fcbits_base = 7
  hypers.hdr_fcbits_extend = 1

  hypers.base_dotsper = 30
  hypers.expon_dotsper = .4

  hypers.verHeader = "dsi v00.0-                            "
  ' -- length (38)   " - - - - - - - - - - - - - - - - - - -"

  seed_hdr.setmax hypers.hdr_cbits

  '' prior to official format release,
  '' change master values (below) to match.

  return @hypers
End function

  
  function master as t_hyper_parameters ptr
    static as t_hyper_parameters hypers

/'
  -- Do you mean to adjust work values?
  -- You probably do.
 
  written file values are
'/

hypers.radScale0 = .14
hypers.radDecay = .49
hypers.radDetailRush = .15

hypers.hdr_cbits = 1
hypers.hdr_fcbits_base = 7
hypers.hdr_fcbits_extend = 1

hypers.base_dotsper = 30
hypers.expon_dotsper = .4
  
hypers.verHeader = "dsi v00.0 (unofficial)                "
' -- length (38)   " - - - - - - - - - - - - - - - - - - -"

return @hypers
End function

  /' -- expected hash algorithm -- '/

dim as dsi_hash.statelit a, b

dim as ulongint mulC = &b10000000001000000001000000010000001000001000010001001011

sub _rotr(byref q as dsi_hash.statelit, amount as byte)
  q = (q shl (dsi_hash.lenx8 - amount)) or (q shr amount)
End Sub

function _valu( i as ulongint = 0) as dsi_hash.statelit '' integer
  b += ((a+i) * mulc) shr 1
  _rotr b, 16
  a xor= 1 + b
  return a
End function

function _ini( i as ulongint = 0, val_a as dsi_hash.statelit = 0, val_b as dsi_hash.statelit = 0) as dsi_hash.statelit
  a = val_a
  b = val_b
  return _valu(i)
End function

function _RNGs_equal int
  for i int = 0 to 1
    var a = rnd * culng(-1)
    var b = rnd * culng(-1)
    if _ini(i, a, b) <> dsi_hash.ini(i, a, b) then return false
  next
  return true
End Function

sub compute__flag_official
  stream.flag_official = _RNGs_equal and (*master = *work_branch)
End Sub
  
  dim as hdr_vars   ghvars

sub ghvars_from_official
  ghvars = type(master)
end sub

const as string str_flag0_bytes = "flag: 0"

end namespace

  
'
'
  namespace dsi_imager

dim int         g_pos_sub_exit
dim int         pos_break

dim as short    g_hash_ini
dim as ubyte    c_seedbits
dim as short    seed_max

dim int         idot_break
dim int         frame

sub _cseedbits_calcs( byref p as hdr_vars ptr)
  
'  ? " cseedbits_calcs";
  var pos_increment = p->p_hypers->hdr_cbits
  g_hash_ini = stream.read( p->p_hypers->hdr_cbits, pos_increment )
  c_seedbits = clamp( seed_hdr.f_cseedbits( p->p_hypers, g_hash_ini ), 14, -(p->p_hypers->hdr_cbits = 0))
 
    dim int dots_per = max( _
  p->p_hypers->base_dotsper, _
  frame ^ p->p_hypers->expon_dotsper)

  '' tricky off-by-1 used by some loops
    pos_break = min( _
  stream.pos_oob + 1 - c_seedbits, _
  stream.bitpos + dots_per * c_seedbits - ((c_seedbits=0)or(dots_per = 0)))
  
  idot_break = p->idot + dots_per
  
  seed_max = 2 ^ c_seedbits - 1
  
'  ?"pos br idot br"; stream.bitpos; pos_break; p->idot; idot_break
end sub



dim as im_downscaler    imv_source
dim as im_downscaler    imv_mid

sub _bmpload_and_downscale( filename as string, scale_amount sng = 0.4 )
  imv_source.bmp_load filename
  if imv_source.h < 1 then exit sub
  imv_mid.downscale_from @imv_source, scale_amount
  infoHeader.avgcol = imv_mid.avg_col
end sub


dim as proc_view procview


sub sbuf_show( y int = 0, x int = 0, str_mesg as string = "")
  procview.sbuf.view 240, 50
  if y > 0 or x > 0  then
    locate y, x
    ? str_mesg
  endif
  screenlock: screenunlock
end sub

sub printstuff( byref p as hdr_vars ptr, title_msg as string = "", cond int = true, paus int = false)
  if cond then
    sbuf_show
    ? title_msg
'    ? "bene "; p->bene qc
'    ? "idot, break"; p->idot; idot_break qc
'    ? "pos, enc, break"; stream.bitpos; p->bits_encoded; g_pos_sub_exit qc
    if paus then sleep
  endif
End Sub



sub _sbuf_solidfill
  var             stren = 1.0
  procview.sbuf.fill infoHeader.avgcol, stren, fill_mode.solid'
end sub
 

dim sng                 gRadMul

sub _workbuf_fullsize( byref p_hypers as t_hyper_parameters ptr)
  var scalar = 1
  procview.work_scale @imv_mid, scalar
  aadot_nosq.render_target @procview.sbuf
  gRadMul = p_hypers->radScale0 * procview.src.diagonal
end sub

 
dim as ns_ecs.t         hashed_props

dim sng                 gMaxRad

dim as v3 sCOL

sub _drawdot(dot_idx int, draw_vs_erase int)
  aadot_nosq._render @hashed_props, _
    type<sRGBi>( _
      sCOL, _
      hashed_props.a * draw_vs_erase )
end sub

sub props_from( byref p as hdr_vars ptr)
  var dna = p->dot_seed and seed_max
  with hashed_props
    
      .ini_ p->idot, g_hash_ini, dna ''  unique stream
                                     '' hash run once seed:  p->idot 
                                     '' a, b initializations:  g_hash, dna
     
      .y.run
      .x.run
      .slope.run 'dna
              
              ' eigenvectors.  order is important - 2022 Mar 4
              
              ' for comparison, comment out  dna

      
      .rad.run dna
      .r.run dna    '' hue
      .g.run dna      '' saturation
      .b.run dna      '' value (hsv)
      .style.run dna
      .a.run dna
' ? "ii ";.b;" ";dna qc
     
      '' final adjustments
      .rad = gRadMul * (.5 + .5 * .rad) * _
        p->p_hypers->radDecay ^ (p->idot ^ p->p_hypers->radDetailRush)
      .x *= procview.src.w
      .y *= procview.src.h
      '.slope = iif(p->idot < 200,.5,_
      '.slope._sval)
      .slope /= .rad
      '.a = 1
     
      sCOL = hsv(.r, .g, .b) * (256 * 2.0) '' not entirely sure why less vibrant around 1.0
  end with
  aadot_nosq.xy01_ @hashed_props
end sub

' --
'
const drawmode_draw = 1
const drawmode_erase = -1

dim dbl       t, tTriggerF

sub _dot_decode( byref p as hdr_vars ptr, dna int)
  p->dot_seed = dna
  props_from p
  _drawdot p->idot, drawmode_draw
end sub

sub _decode_chunk( byref p as hdr_vars ptr)
 
  _cseedbits_calcs p
 /' 
? "dec chunk" qc
    qq; p->bits_encoded qc
sleep'/
  while stream.bitpos < pos_break andalso p->idot < idot_break
    _dot_decode p, stream.read( c_seedbits, c_seedbits )
    p->idot += 1
  wend
  
  #if 0 '' debugger
  if gmode=2 then
    sbuf_show
    ? "bp, break";stream.bitpos; pos_break
    ? "hdr "; g_hash_ini
    sleep
  EndIf
  #endif
end sub


sub _decode( byref progress as hdr_vars ptr = 0)
  'qpp " _deco"
  if procview.h < 1 then exit sub

  _sbuf_solidfill
  
  if progress = 0 then progress = @ns_compat.ghvars
  progress->idot = 0
  frame = 0
  stream.bitpos = stream.c_flagbits
  '? stream.bitpos
  while stream.bitpos < progress->bits_encoded
    _decode_chunk progress
    frame += 1
  wend

  #if 0 '' debugger
  if gmode=2 then
    sbuf_show
    ? "bp, be, sub exit";stream.bitpos; progress->bits_encoded; g_pos_sub_exit qc
    sleep
  EndIf
  #endif
end sub

' ------------

sub _calc_dot_bene( byref p as hdr_vars ptr)
  var error_nodot = AaDot_noSq.circ_err( @procview, @hashed_props )
  
  _drawdot p->idot, drawmode_draw
  var error_dot = AaDot_noSq.circ_err( @procview, @hashed_props )
  
  var improvement = error_nodot - error_dot
  p->bene = improvement
end sub

sub _encode_dot( byref p as hdr_vars ptr, draw_vs_erase int)
  if draw_vs_erase = drawmode_draw then
    props_from p
    _calc_dot_bene p
  else '' erase
    props_from p
    _drawdot p->idot, drawmode_erase
  endif
end sub

function _dot_result(byref p as hdr_vars ptr, dot_seed int) as hdr_vars
  p->dot_seed = dot_seed
  _encode_dot p, drawmode_draw
  _encode_dot p, drawmode_erase
  return *p
end function

sub _best_dot( byref p as hdr_vars ptr)
  var dna = 0
  var dot_best = _dot_result(p, dna)
  
  for dna = 1 to seed_max
    var dot = _dot_result( p, dna )
    if dot.bene > dot_best.bene then dot_best = dot
  next

  *p = dot_best
end sub

sub _encode_dots( byref p as hdr_vars ptr, hdr_val as byte )
  '? " enc dots"
  var pos0 = stream.bitpos
  
  var advance_amount = 0 '' stresm.bitpos advances in _cseedbits_calcs from stream.read() call
  stream.write hdr_val, p->p_hypers->hdr_cbits, advance_amount
  
  'qq; p->bits_encoded qc
  _cseedbits_calcs p
  'qq; " -" qc
  
  dim dbl sum_bene
  
  '? "c seedbits "; c_seedbits qc
  '? "pos break"; pos_break qc
  while stream.bitpos < pos_break andalso p->idot < idot_break
    _best_dot p
    sum_bene += p->bene
    
    var advance_amount = 0
    stream.write p->dot_seed, c_seedbits, advance_amount
    'qq; p->bits_encoded qc
    advance_amount = c_seedbits
    var dot_seed = stream.read( c_seedbits, advance_amount)

    _dot_decode p, dot_seed
    '? p->idot;
    p->idot += 1
  wend

  p->bits_encoded = stream.bitpos
  p->dots_encoded = p->idot
  p->bene = sum_bene / (stream.bitpos - pos0)
  
  /'
  'var paus = true
    '? p->idot; idot_break qc
    qq
    sleep
  'printstuff p, " encdots", gmode, paus
  '/
End Sub


dim as byte   dna_best()
dim int       byPos1       '' 2022 Mar 1

function _encode_chunk(byref progress as hdr_vars ptr, hdr_val as byte) as hdr_vars ptr
  static as hdr_vars result:  result = *progress
  stream.bitpos = result.bits_encoded
  _decode @result
  
'  var advance_amount = 0
'  stream.write hdr_val, seed_hdr.cbits, advance_amount
  
  _encode_dots @result, hdr_val
  byPos1 = stream.bitpos\8

  return @result
end function

sub _find_header_best( byref progress as hdr_vars ptr)
  if progress->bits_encoded >= g_pos_sub_exit then exit sub
  
  bypos1 = 0
  var byPos0 = stream.bitpos \ 8
  var header_val = 0
  var hvars_best = *_encode_chunk( progress, header_val)
  
  ' -- copy generated dna (stored as bytes)
  _copy_bytes dna_best(), stream.bytes(), bypos0, bypos1 '' dest, src, 0, 1

  var byPos1_best = byPos1
  
  for header_val = 1 to seed_hdr._max
    
    var result = _encode_chunk( progress, header_val)
    if result->bene > hvars_best.bene then
      
      hvars_best = *result
      bypos1_best = bypos1

      ' -- copy new dna
      _copy_bytes dna_best(), stream.bytes(), bypos0, bypos1
    endif
  next
  
  *progress = hvars_best

  ' -- save best generated dna
  _copy_bytes stream.bytes(), dna_best(), bypos0, bypos1_best

  stream.bitpos = progress->bits_encoded
  g_pos_sub_exit = seed_hdr.f_possubexit( progress->p_hypers, stream.bitpos)
  
  frame += 1
end sub


dim as string g_key

sub _scholastic( diagonal sng, byref progress as hdr_vars ptr, small_rad_exit int = true)  
  procview.work_scale @imv_mid, diagonal / imv_mid.diagonal
  aadot_nosq.render_target @procview.sbuf
  gRadMul = progress->p_hypers->radScale0 * procview.src.diagonal

  while ( progress->bits_encoded < g_pos_sub_exit)
    _find_header_best progress
    '? " schol" qc
    if small_rad_exit then
      if hashed_props.rad < 5.8 then exit while
    EndIf
    
    g_key = inkey
    if g_key <> "" then exit sub
    
    #if 1
     t = timer
    if t > tTriggerF + .5 then
      sbuf_show 20, 2, "bytes left: "+ str(ubound(stream.bytes)+1 - ((stream.bitpos \ 8))) + " "
      tTriggerF = t
      'sleep 30
    endif
    #endif
    
  wend
  '? " schol end"
end sub

sub encode( filename as string, byte_len int )
  
  ' ---- setup -----------
  '
  _bmpload_and_downscale filename, 1.0
  if imv_source.h < 1 then ? ".bmp load error": sleep 1200: exit sub
 
  if byte_len > ubound(stream.bytes)+1 then redim stream.bytes(byte_len - 1)
  
  var u = ubound(stream.bytes)
  redim dna_best( u)

  stream.pos_oob = ( u+1) * 8
  g_pos_sub_exit = stream.pos_oob
 
  dim as hdr_vars _
    progress = type(ns_compat.work_branch)
  
  ns_compat.compute__flag_official
  if stream.flag_official = false then ? ns_compat.str_flag0_bytes

  stream.bitpos = 0
  stream.flags_write
  
  t = timer:  tTriggerF = t - 10
  '
  ' ------ setup
  
  '  _scholastic is where the magic begins
  '
  progress.bits_encoded = stream.bitpos
  var diagonal = 60
  
  'gmode = 1 '' debugger
  _scholastic diagonal, @progress
  '
  while (diagonal < imv_source.diagonal - 70) andalso stream.bitpos < g_pos_sub_exit
    if g_key <> "" then exit sub
    diagonal += 35
    _scholastic diagonal, @progress
  wend
  '/
'? rnd:sleep
  var b_small_rad_exit = false
  _scholastic imv_source.diagonal, @progress, b_small_rad_exit
  
  'sleep 
  _workbuf_fullsize progress.p_hypers
  ns_compat.ghvars = progress
  
' gmode = 2
'  ? u
'  print_bytes @stream.bytes(0), u
  sbuf_show 20, 2, "bytes left: 0  "

end sub

sub _fill_infoHeader
  with infoheader
  .w = imv_mid.w
  .h = imv_mid.h
  .avgcol = imv_mid.avg_col
  end with
end sub

sub dsi_save(byval filename as string)
  if procview.h < 1 then exit sub
  adjust_filename_dsi filename
  '_define_verHeader
  _fill_infoHeader
  var k = freefile
  open filename for binary access write as #k
    put #k,, ns_compat.ghvars.p_hypers->verHeader  ' -- compiler warning should be okay.
                        ' _define_verHeader sets the string
    put #k,, infoHeader
    put #k,, stream.bytes()
  close #k
end sub

sub decode( show int = true)
  ns_compat.compute__flag_official
  if stream.flag_official = false then ?:? "code: unofficial build"
  
  stream.bitpos = 0
  stream.flags_read
  
  _decode
  if show then sbuf_show
 
  if stream.flag_official = false then ? ns_compat.str_flag0_bytes
end sub


sub dsi_load(byval filename as string)
  adjust_filename_dsi filename
  ns_compat.ghvars_from_official
  
  dim as typeof(ns_compat.ghvars.p_hypers->verHeader)  vh
  
  var k = freefile
  open filename for binary access read as #k
   
    get #k,, vh
    
    #if 1 ' enable for official or debug
          ' disable during dev (hyperparameters tweak, etc)
    
      if ns_compat.ghvars.p_hypers->verHeader <> vh then
        ? "possible compatibility issue"
        ? "file version: "; vh'.dsiHeader
        'sleep 2000
        'close #k: exit sub
      end if
    
    #endif
   
    var u = lof(k) - ns_fileheader.size - 1
    
    redim stream.bytes(u)
   
    get #k,, infoHeader
    get #k,, stream.bytes()
  close #k
  
  imv_mid.create infoHeader.w, infoHeader.h

  ns_compat.ghvars = type(ns_compat.work_branch)
  _workbuf_fullsize ns_compat.ghvars.p_hypers
  stream.pos_oob = (u+1)*8
  ns_compat.ghvars.bits_encoded = stream.pos_oob
  
end sub

end namespace
'
' ------- dsi version.bas


'
' main
'

var w = 800
var h = 700

screenres w, h, 32

using dsi_imager

chdir exepath

#if 1
  
  var data_size = 350
 
  encode filename, data_size
  
  '' "encode finished" cue 
  line(0,0)-(w,h),rgb(99,88,77), bf
  
  '' This works
  'dsi_save filename
  
#else
  dsi_load filename
#endif

decode
?
? "data area: "; ubound(stream.bytes)+1; " bytes"
sleep
