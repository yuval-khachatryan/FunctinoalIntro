#r "D:/Documents/LearnF#/Functional Programming using F#/FunctinoalIntro/Chapter7Modules/bin/Debug/netstandard2.0/Chapter7Modules.dll"
open MyLibrary
open System.Drawing
open System.Windows.Forms
Application.EnableVisualStyles

let h0 = Curve.point(0.0, 0.0)

let hilbert hn = 
    let w = Curve.width hn
    let h = Curve.height hn
    let c0 = hn >< 0.0
    let c1 = c0 |^ -90
    let c2 = hn --> (0.0, w + 1.0)
    let c3 = hn --> (h + 1.0, w + 1.0)
    let c4 = (c0  |^ 90) --> (h + h + 1.0, w)
    c1 + c2 + c3 + c4


let winSize = Size(1000, 500);

let display(title: string, (c:Curve.Curve, pw: int, ph: int)) =
    let f(x,y) = Point(int(round x), ph - int(round y))
    let clst = Curve.toList c
    let Ptlst = List.map f  clst
    let pArr = Array.ofList Ptlst

    let pen = new Pen(Color.Black)
    let draw(g: Graphics) = g.DrawLines(pen, pArr)

    let panel = new Panel(Dock=DockStyle.Fill)
    panel.Paint.Add(fun e -> draw(e.Graphics))

    let win = new Form(Text = title, Size = winSize, AutoScroll = true, 
                       AutoScrollMinSize = Size(pw, ph))
    win.Controls.Add(panel)
    win.Show()

let adjust(c: Curve.Curve, a:float) =
    let c1 = a * c --> (10.0, 10.0)
    let (_, (maxX, maxY)) = Curve.boundingBox c1
    let pw = int (round maxX) + 20
    let ph = int (round maxY) + 20
    (c1, pw, ph)

let rec hilbertCurve n = 
    if n <= 0 then h0 else hilbert (hilbertCurve (n-1) )

display("Hilbert Curve 6", adjust(hilbertCurve 3, 10.0))