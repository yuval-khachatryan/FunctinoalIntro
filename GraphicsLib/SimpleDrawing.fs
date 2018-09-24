namespace GraphicsLib
open SolutionsLib
open System.Drawing
open System.Windows.Forms


module SimpleDrawing = 
    
    Application.EnableVisualStyles()
    let winSize = Size(450, 300)
    
    let display(title: string, (c : Curve.Curve, pw: int, ph: int) ) =
        let f(x,y) = Point(int(round x), ph - int (round y))
        let clst  = Curve.toList c
        let ptLst = List.map f clst
        let pArr = Array.ofList ptLst

        let pen = new Pen(Color.Black)
        let draw(g: Graphics) = g.DrawLines(pen, pArr)

        let panel = new Panel(Dock = DockStyle.Fill)
        panel.Paint.Add(fun e -> draw(e.Graphics))

        let win = new Form(Text = title, Size = winSize, AutoScroll = true, 
                           AutoScrollMinSize = Size(pw,ph))
        win.Controls.Add(panel)
        win.Show()
    
    let adjust(c: Curve.Curve, a:float) =
        let c1 = a * c --> (10.0, 10.0)
        let (_, (maxX, maxY)) = Curve.boundingBox c1
        let pw = int (round maxX) + 20
        let ph = int (round maxY) + 20
        (c1, pw, ph)