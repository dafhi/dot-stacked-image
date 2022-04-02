'
'  give this thing a .bmp !
'
var filename = ".bmp"

'#include "t/version.bas"
/' ------ version.bas - 2022 April 1 - by dafhi ---------------
  
  "dot stacked image" lossy format.  version-specific codez

  - saved file compatibility not guaranteed
 
   - ---------
    updates
  -----------
  April
  1 - new hyper params, moved code around
  
	March
  31 - tossed hyper-parameter radDetailRush
  28 - variable len header text
  27 - Stream and Base128 (namespaces) to boilerplate.bas
  18 - slow hypers for 'extreme' compression. elusive bug somewhere :\
  17 - Base128 Enc/Dec (testing)
  [update]
  renamed .hdr_fcbits_extend -> .hdr_fcbits_varied
  fixed a problem in seed_hdr.f_cseedbits() .. overall quality improved

  ==================================================
  
  --------------------
   algorithm overview
  --------------------
 
  1. anti-aliased dot with properties  x y sRGBA rad slope
  2. hash function init w/ seeds (dot_index, dna), then run once
  3. each property also runs hash.  some are eigenvectors.  see:  props_from
  4. to converge the image, compare all seeds 0 to 2^( c_seedbits: ~ 1 to 3 ) - 1
    A. before and after measurement of dot vs. image under same dot area
    B. save seed most-ish improving the render
    
  important distinctions about FreeBASIC
  1. true = -1
  2. 0.5 to int = 1
  
  some cpu architectures run about 75% slower.
  The bottleneck appears to be sRGBi.Cast.

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

'#include "../gfx backend.bas"
' ------ gfx backend.bas - 2022 March 20 - by dafhi ---------------
'

'#include "boilerplate.bas"
' ------ dsi boilerplate.bas - 2022 March 31 - by dafhi ---------------
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

sub cpy( pdes as any ptr, psrc as any ptr, c int)
  dim as byte ptr _des = pdes, src = psrc
  fb_memcopy *_des, *src, c
End sub

function pos_val0( p as any ptr ) int
  dim as byte ptr a = p
  while *a <> 0
     a += 1
  wend
  return a - cast( byte ptr, p)
end function

const return_key = 13 '' ?
const c10 = chr(10) '' line feed?
const c34 = chr(34) '' double quote


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


sub cpy_byt_arys(des() as byte, src() as byte, lo int, hi int)
  fb_memcopy des(lo), src(lo), hi + 1 - lo
end sub

function sbin(p as any ptr, cBytes as longint = 1) as string
  var s = ""
  gp.a = p + cbytes - 1
  for j as longint = 1 to cBytes
    for i as long = 7 to 0 step -1
      s += str((*gp.b shr i) and 1)
    next:  gp.a -= 1
  next
  return s
end function


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

sub adjust_file_ext( byref filename as string, _extension as string = ".txt" )
  filename = left(filename, len(filename) - 4) + _extension
end sub


type Rect1 '' used by aadot_nosq.draw but placed here for debug
  int                 X0, Y0, X1, Y1
  decl oper           cast as string
End Type

oper Rect1.cast as string
  return str(x0) & " " & str(y0) & " " &_
    str(x1) & " " & str(y1) & " "
end oper


dim shared as Rect1          rc



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

 
  namespace ns_fileheader '' 2022 March 13

type infoHeader field = 1 '' byte align
  
  'as string   text '' 2022 Mar 13
  
  as ushort       wm
  as ushort       hm
  as RGB24        avgcol
  
  decl prop       w as short '' 2022 Mar 12
  decl prop       h as short
  decl prop       w( as short)
  decl prop       h( as short)
 
end type

prop infoHeader.w( param as short)
  wm = clamp(param, 65536, 1) - 1
end prop

prop infoHeader.h( param as short)
  hm = clamp(param, 65536, 1) - 1
end prop

prop infoHeader.w as short
  return wm + 1
end prop

prop infoHeader.h as short
  return hm + 1
end prop

end namespace


  namespace stream

dim as long     bitpos, _pos_oob
dim as ubyte    bytes()

dim int         b_offset
dim as ulong    mask

function _stream_common(cbits as byte, bitpos_inc int) int
  if bitpos + bitpos_inc > _pos_oob then
    bitpos += bitpos_inc: return true
  else
    var bypos = bitpos \ 8 '' integer divide
    b_offset = bitpos - bypos * 8
    gp.a = @bytes(bypos)
    mask = (2 ^ cbits - 1)
    bitpos += bitpos_inc
    return false
  endif
end function

sub write(valu as short, cbits as byte, bitpos_inc as short)
  if _stream_common( cbits, bitpos_inc) then exit sub
  *gp.l and= -1 xor (mask shl b_offset) '' 4 byte int ptr
  *gp.l or= (valu and mask) shl b_offset
end sub

function read( cbits as byte, bitpos_inc as short) as short
  if _stream_common( cbits, bitpos_inc) then return -1
  return (*gp.l shr b_offset) and mask
end function
  
  const   total_flagbits = 1
  const   pos_advance = 1
  
  dim int flag_official   '' "official version"

sub flags_write
  write( flag_official, 1, pos_advance )
end sub

sub flags_read
  flag_official = read( 1, pos_advance )
end sub

end namespace ' --- stream

  namespace Base128

dim as string*32 q0 = "`abcdefghijklmnopqrstuvwxyz{|}~0"
dim as string*32 q1 = "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_"
dim as string*32 q2 = "àáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"
dim as string*32 q3 = "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞß"

dim as string * 128 r
dim as byte         ref(255)

sub _fill_ref_array
  r = q0 + q1 + q2 + q3
  for i int = 0 to 127
    ref( r[i] ) = i
  Next
End Sub

function encode( bytes() as byte ) as string
  r = q0 + q1 + q2 + q3
  
  redim stream.bytes( ubound(bytes))
  cpy @stream.bytes(0), @bytes(0), ubound(bytes)+1
  
  stream._pos_oob = (ubound(bytes)+1) * 8
  var cbits_rescaled = (stream._pos_oob \ 7) * 8
  var cbytes_rescaled = cbits_rescaled \ 8 '' integer divide
  dim as string q = space( cbytes_rescaled )

  const cbits = 7
  const pos_advance = 7
  stream.bitpos = 0
  dim int i
  while stream.bitpos < stream._pos_oob + 1 - cbits '' off by 1's  >.<
    q[i] = r[ stream.read( cbits, pos_advance ) ]
    i += 1
  wend
  return q
end function

function decode( s as string ) as string
  _fill_ref_array
  stream._pos_oob = len(s) * 7 
  redim stream.bytes( stream._pos_oob \ 8) '' integer divide
  
  const stream_cbits = 7
  const pos_advance = 7
  stream.bitpos = 0
  dim int i
  while stream.bitpos < stream._pos_oob + 1 - stream_cbits '' off by 1's  >.<
    stream.write ref( s[i]), stream_cbits, pos_advance
    i += 1
  wend
  
  dim as string q = space( ubound(stream.bytes) + 1 )
  cpy @q[0], @stream.bytes(0), len(q)
  
  return q  
end function
    
End Namespace

  
  namespace dbg

dim int frame, calorie, cb_encoded, cd_encoded, cb_dot, dotsper
dim int pos, break_loop, break_oob, oob

  sub p( mesg as string = "")', reset_line int = true)
'if reset_line then locate 1,1
? mesg; "  ";
? "frame"; frame;
? "  bits_enc"; cb_encoded;
? "  dots_enc"; cd_encoded;
? "  cb_dot"; cb_dot;
? "  calorie"; calorie;
? "  dots_per"; dotsper;
? "  pos"; pos;
? "  break loop"; break_loop;
? "  break oob"; break_oob;
? "  oob"; oob
'? " "; 
End Sub

sub fill_i( fra int = -1, cb int = -1, cd int = -1)
  if fra > -1 then frame = fra
  if cb > -1 then cb_dot = cb
  if cd > -1 then dotsper = cd
End Sub

sub fill_o( po int = -1, brl int = -1, br int = -1)
if po > -1 then pos = po
if brl > -1 then break_loop = brl
if br > -1 then break_oob = br
End Sub

End Namespace
'
' --------  boilerplate.bas


' --- gfx backend.bas continued ..
'
type imagevars
 
  decl                  csr( as any ptr = 0) '' constructor
  declare               destructor
  decl oper             cast as any ptr
  declare sub           acquire( as any ptr = 0 )    '' example:  acquire imagecreate(400, 300)
  declare sub           scan( as any ptr = 0 )    '' example:  scan imagecreate(400, 300)
  declare sub           create(w int=0, h int=0, col as ulong=&HFF000000)
  declare sub           bmp_load( ByRef filename As String )
  declare sub           blit(x int=0, y int=0, size as ubyte=0, byref pdest as any ptr=0)
 
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
  acquire im
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

sub imagevars.scan( _im as any ptr) '' 2022 Mar 15
  if (_im = 0) orelse (_im = screenptr) then
    ScreenInfo w,h, bpp, bypp, pitch, rate, driver_name:  _
    pixels = screenptr:  _specialized
  elseif Imageinfo(_im) = 0 then
    ImageInfo _im, w, h, bypp, pitch, pixels: _
    bpp = bypp * 8:  _specialized
  endif
  im = 0 '' avoids imagedestroy
end sub

sub imagevars.acquire( _im as any ptr) '' 2022 Mar 15
  if (_im = 0) orelse (_im = screenptr) then
    release:  ScreenInfo w,h, bpp, bypp, pitch, rate, driver_name
    pixels = screenptr:  im = 0:  _specialized
  elseif Imageinfo(_im) = 0 then
    release:  im = _im: ImageInfo im, w, h, bypp, pitch, pixels:
    bpp = bypp * 8:  _specialized
  endif
end sub

sub imagevars.create( _w int, _h int, col as ulong)
  _w = abs(_w) '' 2021 Dec 19
  _h = abs(_h)
  if _w<1 or _h<1 then exit sub
  acquire imagecreate(_w,_h,col) '' 2022 Mar 15
End Sub

sub imagevars.blit( x int, y int, size as ubyte, byref pdest as any ptr) '2022 Mar 16
  
  if size > 1 then
    
    static as imagevars vdes
    
    var sizem=size-1
    if pdest=0 then vdes.scan': pdest=@dest
    var x1=x+wm*size: if x1>vdes.wm then x1=vdes.wm
    var y1=y+hm*size: if y1>vdes.hm then y1=vdes.hm
   
    for iy as long=y to y1 step size
      dim as ulong ptr psrc = p32 + ((iy-y)\size) * pitchBy
      if pdest=0 or vdes.im=0 then
        for ix as long=x to x1 step size
          line (ix,iy)-(ix+sizem,iy+sizem),psrc[(ix-x)\size],bf:  next
      else
        for ix as long=x to x1 step size
          line vdes.im,(ix,iy)-(ix+sizem,iy+sizem),psrc[(ix-x)\size],bf:  next
      endif
    next
    
  else
    
    if pdest = 0 orelse pdest = screenptr then
      put (x,y), im, pset
    elseif imageinfo(pdest)=0 then
      put pdest, (x,y), im, pset
    endif
    
  endif
  
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
    scr.scan
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
 
  des.scan im
'  ? des.im, des.wm: sleep 500
  clip_calc @des, x, y, @rc_des
  
  clip_x0 -= rc_des.x0 '' 2022 Mar 15
  clip_y0 -= rc_des.y0

  for y = rc_des.y0 to rc_des.y1
    'if y < 1 or y >= hm then ? y
    for x = rc_des.x0 to rc_des.x1
      des.p32[y * des.pitchBy + x] = mydata(x + clip_x0, y + clip_y0)
        next: next
       
end sub


'' helper
dim shared as ubyte                    magnification = 1

type proc_view
  as im_downscaler  src
  as sRGBi_buf      sbuf
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
' ------- gfx backend.bas

 
  namespace dsi_hash '' RNG

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
  
  '' 2022 Mar 24 (note to self:  must match ns_compat)
  #if 0
    b += (a xor i)
    a += (b * mulc) shr 1
  #else
    b += ((a+i) * mulc) shr 1
    rotr b, 16
    a xor= 1 + b
  #endif
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
  decl            csr (sng = 1, sng = 0)
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
 
  decl sub            ini_( int = 0, as dsi_hash.statelit = 0, as dsi_hash.statelit = 0)

  as nBitHashFloat    style = type(0.56, 0)
  as nBitHashFloat    slope = type(1, .501)
  as nBitHashFloat    rad = type(1, 0)
  as nBitHashFloat    x = type(1, 0)
  as nBitHashFloat    y = type(1, 0)
  as nBitHashFloat    r = type(6, 0) '' hue sat val :p
  as nBitHashFloat    g = type(1, 0)
  as nBitHashFloat    b = type(1, 0)
  as nBitHashFloat    a = type(1.6, 0.7) '' 
 
  decl prop           full ac ulong
 
  dbl                 bitpos_s, bitpos_delta
  int                 maxinc_frame
 
End Type

sub t.ini_( idx int = 0, seeda as dsi_hash.statelit = 0, seedb as dsi_hash.statelit = 0)
  dsi_hash.ini idx, seeda, seedb
end sub

oper t.cast ac string:  static as string s
  s = "xy " + str(x) + " " + str(y) + " " + c10 + _
   "rad "+ str(rad) + " " + c10 + _
   "r "+ str(r) + " " + c10 + _
   "g "+ str(g) + " " + c10 + _
   "b "+ str(b) + " " + c10 + _
   "a "+ str(a) + " " + c10 + _
   "sl "+ str(slope) + " "
   return s
end oper
 
end namespace


 /' -- anti-aliased rendering primitive -- '/

  namespace AaDot_noSq '' not using sqr

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

dim int circ_err_final_cplx

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
  
  return iif(circ_err_final_cplx, (sum+.5) / (cPels+.5) ^ .1, sum)
end function


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

  select case ac j '' ac = as const
  
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




type t_hyper_parameters field = 2 '' 16 bit align
  sng       radScale0
  sng       radDecay
  'sng       radDetailRush
  sng       expon_dotsper
  as byte   base_dotsper
  as byte   hdr_cbits
  as byte   dot_fcbits_base
  as byte   dot_fcbits_varied
  as string text
End Type

oper = (l as t_hyper_parameters, r as t_hyper_parameters) int
  dim as any ptr pal = @l, par = @r
  dim as byte ptr pbl = pal, pbr = par
  
  static int diff
  for i int = 0 to len(t_hyper_parameters)-1
    diff += abs(pbl[i] - pbr[i])
  next
  
  diff += abs( len(l.text) - len(r.text) )
  for i int = 0 to min(len(l.text), len(r.text)) - 1
    diff += abs( l.text[i] - r.text[i] )
  next
  
  return diff = 0
end oper


  namespace seed_hdr
  
dim as short    _max

function f_cdotbits( byref p_hypers as t_hyper_parameters ptr, val as byte) as byte
  dim int varied = p_hypers->dot_fcbits_varied <> 0 '' 0 or -1
  return p_hypers->dot_fcbits_base - varied * max(val, _max)
end function

/'  cbits [incl header] to plot first dot in a chunk  '/
function f_calorie( byref p_hypers as t_hyper_parameters ptr, val as byte) as byte
  return iif( _ '' ret > 0
    p_hypers->hdr_cbits = 0, _
      max( p_hypers->dot_fcbits_base, 1), _
      p_hypers->hdr_cbits + f_cdotbits( p_hypers, val) )
End Function

sub setmax( hdr_cbits int)
  _max = min(2 ^ hdr_cbits - 1, 3) '' keeps speed reasonable when hyper_parameters dot_fcbits_base > 3
End Sub

end namespace
  

type tracking_vars
  decl csr      (byref as t_hyper_parameters ptr = 0)
  as byte       dot_seed
  int           idot
  int           bits_encoded
  int           dots_encoded
  dbl           bene
  
  as t_hyper_parameters ptr _
    p_hypers
end type

csr tracking_vars(byref p as t_hyper_parameters ptr)
  p_hypers = p
end csr


  
  namespace ns_compat

declare sub dependent_vars(byref p as t_hyper_parameters ptr)

/' -- "official" detection --
 
    intent:
  1. group hyperparameters
  2. coder notification
  
  if work_branch & master don't match, "official" flag will be set to 0.

'/
  
  function work_branch() as t_hyper_parameters ptr
    
    static as t_hyper_parameters  hypers

  aadot_nosq.circ_err_final_cplx = 0  '' experimental.
                                      '' may become t_hyper_parameters member


/' -- work_branch() intended for experimentation '/
  
  #if 0
  hypers.radScale0 = .048 '' these are not nearly ideal.  for testing.
  hypers.radDecay = .99944

  hypers.hdr_cbits = 4
  hypers.dot_fcbits_base = 2'5

  hypers.base_dotsper = 6
  hypers.expon_dotsper = .1
  #else
  hypers.radScale0 = 0.046
  hypers.radDecay = .99983

  hypers.hdr_cbits = 1
  hypers.dot_fcbits_base = 7

  hypers.base_dotsper = 7
  hypers.expon_dotsper = 1.4
  #endif
  
  hypers.dot_fcbits_varied = 1'hypers.hdr_cbits < 4
  
  '' for official format release,
  '' master() must match work_branch()

  hypers.text = "dsi v00.0 - testing .."

  
  '' encoder runs off work_branch() values
  dependent_vars @hypers
  
  return @hypers
  
End function

  
  function master as t_hyper_parameters ptr
    static as t_hyper_parameters hypers

/'
  -- Do you mean to adjust work_branch?
  -- You probably do.
 
  saved file generated from work_branch
'/

  hypers.radScale0 = .067 '' these are not nearly ideal.  for testing.
  hypers.radDecay = .99935

  hypers.hdr_cbits = 2
  hypers.dot_fcbits_base = 5
  hypers.dot_fcbits_varied = hypers.hdr_cbits < 4

  hypers.base_dotsper = 20
  hypers.expon_dotsper = .4

/'
  -- Do you mean to adjust work_branch?
  -- You probably do.
'/

hypers.text = "dsi v00.0 - pygmi - we're live! "

return @hypers
End function


sub dependent_vars(byref p as t_hyper_parameters ptr)
  seed_hdr.setmax p->hdr_cbits
End Sub

  
/' -- expected hash algorithm -- '/

dim as dsi_hash.statelit a, b

dim as ulongint mulC = &b10000000001000000001000000010000001000001000010001001011

sub _rotr(byref q as dsi_hash.statelit, amount as byte)
  q = (q shl (dsi_hash.lenx8 - amount)) or (q shr amount)
End Sub

function _valu( i as ulongint = 0) as dsi_hash.statelit '' integer
  
  '' 2022 Mar 24 - (note to self:  match definition at file start)
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
' -----------------

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

const as string q_flag0 = "stream flag:  0 - unofficial"

end namespace

  
'
'
  namespace dsi_imager

dim as ns_fileheader.infoHeader infoHeader

dim int         pos_break_chunk, idot_break

dim as short    g_hash_dna
dim as ubyte    c_seedbits
dim as short    seed_max

dim int         frame, dots_per

sub _cseedbits_calcs( byref p as tracking_vars ptr)
  
  var pos_increment = p->p_hypers->hdr_cbits
  g_hash_dna = stream.read( p->p_hypers->hdr_cbits, pos_increment )
  'if g_hash_dna < 0 then exit sub
  c_seedbits = seed_hdr.f_cdotbits( p->p_hypers, g_hash_dna )
 
  dim int a = p->p_hypers->base_dotsper
  dim int b = frame ^ p->p_hypers->expon_dotsper
  dots_per = max( a, b)
  
  a = stream._pos_oob - c_seedbits
  b = stream.bitpos + dots_per * c_seedbits
  pos_break_chunk = min( a, b) + 1 '' off-by-2 ftw.  _pos_oob is an off-by-1
  
  idot_break = p->idot + dots_per
  
  seed_max = 2 ^ c_seedbits - 1
  
end sub


dim as im_downscaler    imv_source
dim as im_downscaler    imv_mid
dim as byte             dna_best()


dim as proc_view procview


sub sbuf_show( y int = 0, x int = 0)
  procview.sbuf.view 0,0
  if y > 0 or x > 0  then
    locate y, x
  endif
  screenlock: screenunlock
end sub



sub _sbuf_solidfill
  var             stren = 1.0
  procview.sbuf.fill infoHeader.avgcol, stren, fill_mode.solid'
end sub
 

dim sng                 gRadMul

sub _workbuf_size( byref p_hypers as t_hyper_parameters ptr, scalar sng )
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

sub props_from( byref p as tracking_vars ptr)
  
  var dna = p->dot_seed and seed_max
  with hashed_props
      
      .style._sngmin = .0 '' flat vs gradient
      .style._sngmax = .507 '' .5 and greater gives flat a chance
    
      .ini_ p->idot, g_hash_dna, dna ''  unique stream:
                                     '' hash run once seed:  p->idot 
                                     '' a, b initializations:  g_hash, dna
     
      .y.run
      .x.run
              
      '' eigenvectors.  most significant last (?) - 2022 Mar 18
      '
      .style.run dna
      .r.run dna      '' hue
      .g.run dna      '' saturation
      .b.run dna      '' value (hsv)
      .rad.run dna
      .slope.run dna
      .a.run dna
      
      '' final adjustments
      .rad = gRadMul * (.45 + .55 * .rad) * _
        p->p_hypers->radDecay ^ (p->idot)' ^ p->p_hypers->radDetailRush)
      .x *= procview.src.w
      .y *= procview.src.h
      .slope /= .rad
      '.style = 0
     
      sCOL = hsv(.r, .g, .b) * (256 * 1.6) '' not entirely sure why less vibrant around 1.0
  
  end with

  aadot_nosq.xy01_ @hashed_props
  
end sub

' --
'
const drawmode_draw = 1
const drawmode_erase = -1

sub _dot_decode( byref p as tracking_vars ptr, dna int)
  p->dot_seed = dna
  props_from p
  _drawdot p->idot, drawmode_draw
end sub

sub _decode_chunk( byref p as tracking_vars ptr)
  _cseedbits_calcs p
  while stream.bitpos < pos_break_chunk andalso p->idot < idot_break
    p->idot += 1
    _dot_decode p, stream.read( c_seedbits, c_seedbits )
  wend  
end sub

dim as tracking_vars progress

dim dbl       t, tTriggerF

sub _bmpload_and_downscale( filename as string, scale_amount sng = 0.4 )
  imv_source.bmp_load filename
  static int bmp_message = true

  ' ---- setup -----------
  if imv_source.h < 1 then
    if bmp_message then ? ".bmp load error": sleep 1200: bmp_message = false
    exit sub
  EndIf
  
  imv_mid.downscale_from @imv_source, scale_amount
  infoHeader.avgcol = imv_mid.avg_col
  infoHeader.wm = imv_source.wm
  infoHeader.hm = imv_source.hm
  progress.p_hypers = ns_compat.work_branch
  t = timer:  tTriggerF = t - 10
end sub


sub _decode

  if procview.h < 1 then exit sub

  _sbuf_solidfill
  
  progress.idot = -1
  frame = 0
  stream.bitpos = stream.total_flagbits
  while stream.bitpos < progress.bits_encoded
    _decode_chunk @progress
    frame += 1
  wend

end sub

function _pixellate_decode( diagonal sng = 0, message as string = "") int
  
  dim sng scale = _
    iif(diagonal > 0, diagonal / imv_source.diagonal, 1)
  
  _workbuf_size progress.p_hypers, scale
  dim int blit_x, blit_y
  
  dim int scr_w, scr_h
  screeninfo scr_w, scr_h
  
  if message = "" then
    line(0,0)-(scr_w,scr_h),rgb(99,88,77), bf '' cls
    scale = 1
  else
    
    scale = 8
    
    blit_x = (scr_w - scale*procview.w) / 2
    blit_y = (scr_h - scale*procview.h) / 2
  EndIf
  
  _decode

  with procview

    .sbuf.view 0,0, .src.im '' .blit (below) allows large pixels ..
                            '' sbuf.view does not
                            
    'line .src.im, (0,0)-(.src.w, .src.h),rgb(5,5,5), bf
    .src.blit blit_x, blit_y, scale
  End With  
'return false

  if message = "" then
    return false
  else
    ? message
    var s = getkey
    return iif( lcase(chr(s)) = "y" or (s = return_key), true, false )
  EndIf
end function

dim int _fresh_from_the_oven


sub decode

  if _fresh_from_the_oven then
  else
    if not _pixellate_decode( 22, "continue?") then exit sub
    if not _pixellate_decode( 28, "how about now?") then exit sub
  EndIf
  
  _pixellate_decode
  ns_compat.compute__flag_official
  
end sub

' encode ------------

sub _calc_dot_bene( byref p as tracking_vars ptr)
  var error_nodot = AaDot_noSq.circ_err( @procview, @hashed_props )
  
  _drawdot p->idot, drawmode_draw
  var error_dot = AaDot_noSq.circ_err( @procview, @hashed_props )
  
  var improvement = error_nodot - error_dot
  p->bene = improvement
end sub

sub _encode_dot( byref p as tracking_vars ptr, draw_vs_erase int)
  props_from p
  if draw_vs_erase = drawmode_draw then
    _calc_dot_bene p
  else '' erase
    _drawdot p->idot, drawmode_erase
  endif
end sub

function _dot_result(byref p as tracking_vars ptr, dot_seed int) as tracking_vars
  p->dot_seed = dot_seed
  _encode_dot p, drawmode_draw
  _encode_dot p, drawmode_erase
  return *p
end function

sub _best_dot( byref p as tracking_vars ptr)
  p->idot += 1
  var dna = 0
  var dot_best = _dot_result(p, dna)
  for dna = 1 to seed_max
    var dot = _dot_result( p, dna )
    if dot.bene > dot_best.bene then dot_best = dot
  next
  *p = dot_best
end sub

'' ----- debugger -----


sub _encode_dots( byref p as tracking_vars ptr, hdr_val as byte )
  var pos0 = stream.bitpos
  
  var advance_amount = 0 '' stresm.bitpos advances in _cseedbits_calcs from stream.read() call
  stream.write hdr_val, p->p_hypers->hdr_cbits, advance_amount
  
  _cseedbits_calcs p
  
  dim dbl sum_bene
  while stream.bitpos < pos_break_chunk andalso p->idot < idot_break
    
    _best_dot p
    sum_bene += p->bene
    
    var advance_amount = 0
    stream.write p->dot_seed, c_seedbits, advance_amount
    
    advance_amount = c_seedbits
    var dot_seed = stream.read( c_seedbits, advance_amount)
    _dot_decode p, dot_seed
  wend

  p->bits_encoded = stream.bitpos
  p->dots_encoded = p->idot
  p->bene = sum_bene / (stream.bitpos - pos0)

End Sub

dim int byPos0


function _encode_chunk( hdr_val as byte) as tracking_vars ptr
  static as tracking_vars result
  _decode
  byPos0 = stream.bitpos \ 8
  result = progress
  _encode_dots @result, hdr_val
  return @result
end function

sub _find_header_best
  
  var header_val = 0
  var hvars_best = *_encode_chunk( header_val)
  
  ' -- copy generated dna (stored as bytes)
  var bypos1 = stream.bitpos \ 8 '' integer divide
  cpy_byt_arys dna_best(), stream.bytes(), bypos0, bypos1 '' dest, src, 0, 1

  for header_val = 1 to seed_hdr._max
    
    var result = _encode_chunk( header_val)

    if result->bene > hvars_best.bene then
      
      hvars_best = *result
      bypos1 = stream.bitpos \ 8
      ' -- copy new dna
      cpy_byt_arys dna_best(), stream.bytes(), bypos0, bypos1
    endif

  next
  
  progress = hvars_best
  
  ' -- save best dna
  cpy_byt_arys stream.bytes(), dna_best(), bypos0, bypos1
  stream.bitpos = progress.bits_encoded
  
  frame += 1
end sub

dim as string g_key


sub _scholastic( diagonal sng, small_rad_exit int = true)  
  if stream._pos_oob < 1 orelse progress.bits_encoded >= stream._pos_oob then exit sub
  
  _workbuf_size progress.p_hypers, diagonal / imv_mid.diagonal
  
  var hdr_val = 0
  
  while ( progress.bits_encoded < stream._pos_oob )
    
    _find_header_best
    
    if small_rad_exit then
      if hashed_props.rad < 3.7 then exit while
    EndIf
    
    g_key = inkey
    if g_key <> "" then exit while
    
    #if 1
      t = timer
      if t > tTriggerF + .5 then
        sbuf_show 1,1
        windowtitle "bytes left: "+ str(ubound(stream.bytes)+1 - ((stream.bitpos \ 8))) + " "
        tTriggerF = t
      endif
    #endif
    
  wend
  
end sub


sub encode( byte_len int )
  progress.p_hypers = ns_compat.work_branch
  
  if byte_len > 0 then
    redim stream.bytes(byte_len - 1)
    
    var u = ubound(stream.bytes)
    redim dna_best( u)
   
    ns_compat.compute__flag_official

    stream._pos_oob = ( u+1) * 8
    
    stream.bitpos = 0
    stream.flags_write
    
    progress.bits_encoded = stream.bitpos
    var diagonal = 54
    
    _scholastic diagonal
    
    while (diagonal < imv_source.diagonal - 70)' andalso stream.bitpos < stream._pos_oob
      if g_key <> "" then exit while
      diagonal += 35
      _scholastic diagonal
    wend

    var b_small_rad_exit = false
    _scholastic imv_source.diagonal, b_small_rad_exit
  
  endif

  #if 0
  locate 30,1
  var c = ubound(stream.bytes) + 1
  ? strbin(@stream.bytes(0), c)
  ? strbin(@dna_best(0), c)
  sleep
  #endif
  
  _fresh_from_the_oven = true
  
end sub

end namespace ' -- dsi_imager
'
' ------- dsi version.bas

  
  namespace file_stuff

dim as byte   bytes()
dim int simulate = false '' if True, doesn't write to disk.  (also doesn't load a properly-saved file)

sub _structs_to_bytes
  dsi_imager.progress = type(ns_compat.work_branch)
  var c_text = len(dsi_imager.progress.p_hypers->text) + 1 '' include termination byte
  '? "to "; c_text:sleep
  redim bytes(c_text + sizeof(dsi_imager.infoheader) + ubound(stream.bytes))
  cpy @bytes( 0 ), @dsi_imager.progress.p_hypers->text[0], c_text
  cpy @bytes( c_text ), @dsi_imager.infoheader, sizeof(dsi_imager.infoheader)
  cpy @bytes( c_text + sizeof(dsi_imager.infoheader) ), @stream.bytes(0), ubound(stream.bytes) + 1
End Sub

sub _structs_from_bytes
  dsi_imager.progress = type(ns_compat.work_branch)
  'var c_text = len(dsi_imager.progress.p_hypers->text) + 1
  var c_text = pos_val0( @bytes(0)) + 1
  '? "from "; c_text: sleep
  dsi_imager.progress.p_hypers->text = space(c_text)
  cpy @dsi_imager.progress.p_hypers->text[0], @bytes( 0 ), c_text
  cpy @dsi_imager.infoheader, @bytes( c_text ), sizeof(dsi_imager.infoheader)
  var c_data = ubound(bytes) + 1 - c_text - sizeof(dsi_imager.infoheader)
  redim stream.bytes(c_data - 1)
  cpy @stream.bytes(0), @bytes( c_text + sizeof(dsi_imager.infoheader) ), c_data
End Sub

dim as string s


sub _read_common( forum_friendly int )
  
  var c = len(s)
  if c < 1 then exit sub
  
  if forum_friendly then
    s = base128.decode(s)
    redim bytes(len(s)-1)
    cpy @bytes(0), @s[0], len(s)
  else
    redim bytes(len(s)-1)
    cpy @bytes(0), @s[0], len(s)
  endif
  
  _structs_from_bytes
  #if 1
    ns_compat.compute__flag_official
    if stream.flag_official = false then
      'draw string (50,50), "file version: " + dsi_imager.progress.p_hypers->text
      '?
    end if
  #endif
  
  using dsi_imager
  
  imv_source.create infoHeader.w, infoHeader.h
  if infoheader.w + infoheader.h > 9000 then exit sub '' 
  stream.bitpos = 0
  
  c = ubound(bytes)+1 - len(dsi_imager.progress.p_hypers->text) - len(infoheader)
  stream._pos_oob = c*8
  dsi_imager.progress.bits_encoded = stream._pos_oob
  
  imv_mid.downscale_from @imv_source, 1.0
  _fresh_from_the_oven = false

End Sub

dim as byte savestream()

const data_width = 65


sub dsi_save(byval filename as string, forum_text int = false)
  if dsi_imager.procview.h < 1 then exit sub
  
  adjust_file_ext filename, iif(forum_text, " (forum text).txt", ".dsi")
  
  _structs_to_bytes
  
  redim savestream(ubound(stream.bytes))
  cpy @savestream(0), @stream.bytes(0), ubound(savestream) + 1
  var save_oob = stream._pos_oob
  
  if forum_text then
    s = base128.encode( bytes() )
    var i = 0, q = ""
    while i < len(s)
      'q += mid(s, i+1, data_width)
      q += "Data " + c34 + mid(s, i+1, data_width) + c34 + c10
      i += data_width
    wend
    redim bytes( len(q) - 1 )
    cpy @bytes(0), @q[0], len(q)
  endif

  if simulate then
    s = space(ubound(bytes)+1)
    cpy @s[0], @bytes(0), len(s)
  else
    var k = freefile
    open filename for binary access write as #k
      put #1,, bytes()
    close #k
  endif
    
  redim stream.bytes( ubound(savestream))
  cpy @stream.bytes(0), @savestream(0), ubound(savestream) + 1
  stream._pos_oob = save_oob

end sub


sub dsi_load(byval filename as string, forum_text int = false)
  adjust_file_ext filename, iif(forum_text, " (forum text).txt", ".dsi")
 'var sav_bp = stream.bitpos
  'if simulate then
    '? len(s)
  'sleep  
  'else
    
    var k = freefile
    open filename for binary access read as #k
    s = space(lof(k))
    get #k,, s
    close #k
    
  'endif

  if forum_text then
    var i = 0, q = ""
    while i < len(s)
      q += mid(s, i + 7, data_width)
      i += data_width + 8 '' "data + ", end double quote, return
    wend
    s = left(q, len(q)-2) '' minus the end quote and return
  endif
  
  _read_common forum_text
  'stream.bitpos = sav_bp
end sub

sub dsi_read_data
  dim as string q:  s = ""
    
  do
    read q
    s += q
  loop until q = ""
  
  var forum_friendly = true
  _read_common forum_friendly
End Sub

End Namespace ' -- file_stuff

' -----------


var w = 960
var h = 700

screenres w, h, 32

chdir exepath

using dsi_imager

var forum_friendly = 1

_bmpload_and_downscale filename, 1.0
  
var data_size = 450
for i int = data_size to data_size'+180 '' there's still a bug

  '? i: sleep 117
  #if 1
    data_size = i
    '? data_size: sleep 100
    encode data_size
    decode
    ? "encode complete."
  #endif

  #if 1 '' save / load
    if 0 then
      ? "save / load test"
      sleep 1500
      line(0,0)-(w,h),rgb(99,88,77), bf ' .. "done." ..
      file_stuff.dsi_save filename, forum_friendly
      file_stuff.dsi_load filename, forum_friendly
      ? "loaded."
    else
      ? "FB Data test"
      sleep 1500
      file_stuff.dsi_read_data
      ? "Data read."
    endif
    sleep 1300
    decode
  #endif

  windowtitle "data area: " + str(ubound(stream.bytes)+1) + " bytes"
  if stream.flag_official = false then locate 70,2: ? ns_compat.q_flag0
next

sleep
