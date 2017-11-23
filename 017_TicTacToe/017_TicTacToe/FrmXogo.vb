Public Class frmXogo
    Dim mLabels(2, 2) As Label 'Matriz de obxetos Label de 3x3 que contén as nosas casillas
    Dim filas(2) As Integer 'Array no que sumamos as filas X vale 1, O vale 4.
    'Nas labels en AccessibleDescription gardo o numero da fila. En accessible name o nome de columna
    'Se a suma dalgunha columna, fila ou diagonal é dous significa que o human pode gañar e hai que taparlle o burato.
    Dim colus(2) As Integer 'Array no que sumamos as columnas X vale 1, O vale 4.
    Dim diago(1) As Integer 'Array no que sumamos as duas diagonais X vale 1, O vale 4.
    Dim turno As Integer = 0 'Variábel enteira para controlar o número do turno
    Dim gameOver As Boolean = False ' Variábel Booleana para controlar cando acaba o xogo
    Dim iniciaMaquina As Boolean = False 'Booleana para controlar cando inica a máquina a partida
    Public Const BLANCO As String = "" 'Constante de String vacío para as casillas vacías
    Dim nivel As Integer = 2 'Nivel de dificultade. Medio por defecto

    'Método de carga do formulario
    Private Sub frmXogo_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        reseteo() 'Chamada ao método que resetea o taboleiro, punutacións etc
        abrirMenu()
    End Sub

    'Método para resetear o taboleiro, puntuacións e variábels de control
    Private Sub reseteo()
        gameOver = False
        turno = 0
        lblTit.Text = "Tic Tac Toe"
        lblTit.Font = New Font(lblTit.Font, FontStyle.Regular)
        lblTit.ForeColor = Color.DimGray
        mLabels(0, 0) = lbl00
        mLabels(0, 1) = lbl01
        mLabels(0, 2) = lbl02
        mLabels(1, 0) = lbl10
        mLabels(1, 1) = lbl11
        mLabels(1, 2) = lbl12
        mLabels(2, 0) = lbl20
        mLabels(2, 1) = lbl21
        mLabels(2, 2) = lbl22
        'Recorremola matriz para poñer cada elemento a vacío
        For i As Integer = 0 To 2
            filas(i) = 0
            colus(i) = 0
            For j As Integer = 0 To 2
                mLabels(i, j).Text = BLANCO
            Next
        Next
        diago(0) = 0
        diago(1) = 0

        If iniciaMaquina Then inicio_the_matrix()
    End Sub

   



    'Método onde se manexa o click de cada casilla
    Private Sub lbl00_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl00.Click, lbl01.Click, lbl02.Click, lbl10.Click,
        lbl11.Click, lbl12.Click, lbl20.Click, lbl21.Click, lbl22.Click

        Dim pulsado As Boolean = False 'Para controlar que non se pulse na mesma casilla varias veces

        If sender.Text = BLANCO And gameOver = False Then
            sender.Text = "X"
            sumador(CInt(sender.AccessibleDescription), CInt(sender.AccessibleName), 1)
            turno = turno + 1
            pulsado = True
        End If

        If turno > 4 Then
            If gameOver = False Then
                Dim ganador As String = compruebaWin()
                If turno = 9 Then
                    gameOver = True
                    If ganador <> "X" Then
                        lblTit.Text = "¯\_(ツ)_/¯"
                        lblTit.Font = New Font(lblTit.Font, FontStyle.Bold)
                        lblTit.ForeColor = Color.Peru
                    End If
                End If

                If ganador = "X" Then
                    lblTit.Text = "WIN!"
                    lblTit.Font = New Font(lblTit.Font, FontStyle.Bold)
                    lblTit.ForeColor = Color.DarkSeaGreen
                    iniciaMaquina = True
                    lblPunWin.Text = CInt(lblPunWin.Text) + 1
                    gameOver = True
                End If
            End If
        End If
        If turno = 1 And iniciaMaquina = False Then
            inicio_the_matrix()

        ElseIf turno > 1 And gameOver = False And pulsado = True Then
            the_matrix()
            pulsado = False
        End If


    End Sub

    Private Sub sumador(ByVal fila As Integer, ByVal columna As Integer, ByVal valor As Integer)
        filas(fila) += valor
        colus(columna) += valor
        If fila = columna Then 'Significa que está na diagonal principal
            diago(0) += valor
        End If
        If fila + columna = 2 Then 'Diagonal oposta
            diago(1) += valor
        End If
    End Sub

    'Método para que a máquina inicia no centro ou nunha esquina random, dependendo de donde empecemos
    Private Sub inicio_the_matrix()
        If mLabels(1, 1).Text = "X" And turno = 1 Then
            Randomize()
            'Xera números rándom entre 1 e 4 para elexir unha das esquinas
            Dim esquina As Integer = CInt(Int((4 * Rnd()) + 1))

            Select Case esquina
                Case 1
                    mLabels(0, 0).Text = "O"
                    sumador(0, 0, 4)
                Case 2
                    mLabels(0, 2).Text = "O"
                    sumador(0, 2, 4)
                Case 3
                    mLabels(2, 0).Text = "O"
                    sumador(2, 0, 4)
                Case 4
                    mLabels(2, 2).Text = "O"
                    sumador(2, 2, 4)
            End Select
        Else
            mLabels(1, 1).Text = "O"
            sumador(1, 1, 4)
        End If
        turno = turno + 1
    End Sub

    'Método onde se realizan os cálculos para que xogue a computadora
    Private Sub the_matrix()
        Select Case nivel
            Case 1
                rellenaBlancoRandom()
            Case 2
                If pensamientoProfundo() = False Then
                    rellenaBlancoRandom()
                End If
            Case 3
                If pensamientoProfundo2() = False Then
                    rellenaBlancoRandom()
                End If
        End Select

        Dim ganador As String = compruebaWin()
        If ganador = "O" Then
            lblTit.Text = "LOSE"
            lblTit.Font = New Font(lblTit.Font, FontStyle.Bold)
            lblTit.ForeColor = Color.IndianRed
            iniciaMaquina = False
            lblPunLose.Text = CInt(lblPunLose.Text) + 1
            gameOver = True
        End If

        If turno = 9 Then
            gameOver = True
            If ganador = "0" Then
                lblTit.Text = "¯\_(ツ)_/¯"
                lblTit.Font = New Font(lblTit.Font, FontStyle.Bold)
                lblTit.ForeColor = Color.Peru
            End If
        End If

    End Sub

    'Funcíon que comproba se hay tres en raia nas filas, columnas ou diagonais
    Private Function compruebaWin() As String
        Dim caracterGanador = "0"

        If diago(0) = 3 Or diago(1) = 3 Then
            caracterGanador = "X"
        Else
            For i As Integer = 0 To 2
                If filas(i) = 3 Or colus(i) = 3 Then
                    caracterGanador = "X"
                    Exit For
                End If
            Next
        End If

        If caracterGanador <> "X" Then
            If diago(0) = 12 Or diago(1) = 12 Then
                caracterGanador = "O"
            Else
                For i As Integer = 0 To 2
                    If filas(i) = 12 Or colus(i) = 12 Then
                        caracterGanador = "O"
                        Exit For
                    End If
                Next
            End If
        End If


        Return caracterGanador
    End Function

    Private Sub rellenaBlancoRandom()
        Dim vacio As Boolean = False

        'Busca unha casilla vacía de forma random
        Do While vacio = False
            Randomize()
            Dim fi As Integer = CInt(Int((3 * Rnd()) + 0))
            Randomize()
            Dim co As Integer = CInt(Int((3 * Rnd()) + 0))

            If mLabels(fi, co).Text = BLANCO Then
                vacio = True
                turno = turno + 1
                If turno = 9 Then gameOver = True
                mLabels(fi, co).Text = "O"
                sumador(fi, co, 4)
            End If
        Loop
    End Sub

    'Calcula onde hay duas X seguidas e se pode tapar o oco para que o xogador non gañe. Devolve TRUE se encontrou opción
    Private Function pensamientoProfundo() As Boolean
        'Booleana de control
        Dim HaHablado As Boolean = False
        'Bucle para recorrela matriz
        For i As Integer = 0 To 2
            For j As Integer = 0 To 2
                Dim c As String = mLabels(i, j).Text 'Valor da cela na que estamos nesa iteracion
                If c = "X" Then 'So iniciamos calculos se atopamos unha X
                    'Temos que buscar nas 8 casillas de arredor(ainda que esto so sucede no caso da central)
                    'i-1 j-1, i-1 j, i-1 j+1, i j-1, i j+1, i+1 j-1, i+1 j, i+1 j+1

                    If dentroMatriz(i - 1, j - 1) Then 'Miramos se a casilla está dentro da matriz
                        If mLabels(i - 1, j - 1).Text = "X" Then
                            If mLabels(2, 2).Text = BLANCO Then
                                escribeYpasaTurno(2, 2, "O")
                                sumador(2, 2, 4)
                                HaHablado = True
                                Exit For 'Saimos do Bucle xa que Pensamiento Profundo xa realizou a súa xogada
                            End If
                        End If
                    End If

                    If dentroMatriz(i - 1, j) Then
                        If mLabels(i - 1, j).Text = "X" Then
                            If mLabels(2, j).Text = BLANCO Then
                                escribeYpasaTurno(2, j, "O")
                                sumador(2, j, 4)
                                HaHablado = True
                                Exit For
                            End If
                        End If
                    End If

                    If dentroMatriz(i - 1, j + 1) Then
                        If mLabels(i - 1, j + 1).Text = "X" Then
                            If mLabels(2, 0).Text = BLANCO Then
                                escribeYpasaTurno(2, 0, "O")
                                sumador(2, 0, 4)
                                HaHablado = True
                                Exit For
                            End If
                        End If
                    End If

                    If dentroMatriz(i, j - 1) Then
                        If mLabels(i, j - 1).Text = "X" Then
                            If mLabels(i, 2).Text = BLANCO Then
                                escribeYpasaTurno(i, 2, "O")
                                sumador(i, 2, 4)
                                HaHablado = True
                                Exit For
                            End If
                        End If
                    End If

                    If dentroMatriz(i, j + 1) Then
                        If mLabels(i, j + 1).Text = "X" Then
                            If mLabels(i, 0).Text = BLANCO Then
                                escribeYpasaTurno(i, 0, "O")
                                sumador(i, 0, 4)
                                HaHablado = True
                                Exit For
                            End If
                        End If
                    End If

                    If dentroMatriz(i + 1, j - 1) Then
                        If mLabels(i + 1, j - 1).Text = "X" Then
                            If mLabels(0, 2).Text = BLANCO Then
                                escribeYpasaTurno(0, 2, "O")
                                sumador(0, 2, 4)
                                HaHablado = True
                                Exit For
                            End If
                        End If
                    End If

                    If dentroMatriz(i + 1, j + 1) Then
                        If mLabels(i + 1, j + 1).Text = "X" Then
                            If mLabels(0, 0).Text = BLANCO Then
                                escribeYpasaTurno(0, 0, "O")
                                sumador(0, 0, 4)
                                HaHablado = True
                                Exit For
                            End If
                        End If
                    End If

                    If dentroMatriz(i + 1, j) Then
                        If mLabels(i + 1, j).Text = "X" Then
                            If mLabels(0, j).Text = BLANCO Then
                                escribeYpasaTurno(0, j, "O")
                                sumador(0, j, 4)
                                HaHablado = True
                                Exit For
                            End If
                        End If
                    End If

                End If


            Next

            If HaHablado Then Exit For

        Next


        Return HaHablado


    End Function


    Private Function pensamientoProfundo2() As Boolean
        Dim pensado As Boolean = False
        'UNA X VALDRÁ 1
        'UN O VALDRÁ 4
        'SI ENCONTRAMOS EN ALGUNA FILA, COLUMNA O DIAGONAL UNA SUMA DE 8, SIGNIFICA QUE LA IA PUEDE GANAR
        'SI ENTONTRAMOS UNA SUMA DE 2 SIGINIFICA QUE EL HUMANO PUEDE GANAR POR TANTO LE TAPAREMOS EL HUECO


        If pensado = False Then
            For i As Integer = 0 To 2
                If filas(i) = 8 Then
                    pensado = True
                    'buscar hueco donde poner circulo en esa fila
                    For f As Integer = 0 To 2
                        If mLabels(i, f).Text = BLANCO Then
                            escribeYpasaTurno(i, f, "O")
                            sumador(i, f, 4)
                            Exit For
                        End If
                    Next
                    Exit For
                End If
            Next

            If pensado = False Then
                For i As Integer = 0 To 2
                    If colus(i) = 8 Then
                        pensado = True
                        'buscar hueco donde poner circulo en esa columna
                        For c As Integer = 0 To 2
                            If mLabels(c, i).Text = BLANCO Then
                                escribeYpasaTurno(c, i, "O")
                                sumador(c, i, 4)
                                Exit For
                            End If
                        Next
                        Exit For
                    End If
                Next

                If pensado = False Then
                    If diago(0) = 8 Then
                        pensado = True
                        'buscar hueco en la diagonal principal
                        If mLabels(0, 0).Text = BLANCO Then
                            escribeYpasaTurno(0, 0, "O")
                            sumador(0, 0, 4)
                        ElseIf mLabels(2, 2).Text = BLANCO Then
                            escribeYpasaTurno(2, 2, "O")
                            sumador(0, 0, 4)
                        End If
                    ElseIf diago(1) = 8 Then
                        pensado = True
                        'buscar hueco en la diagonal secundaria
                        If mLabels(0, 2).Text = BLANCO Then
                            escribeYpasaTurno(0, 2, "O")
                            sumador(0, 2, 4)
                        ElseIf mLabels(2, 0).Text = BLANCO Then
                            escribeYpasaTurno(2, 0, "O")
                            sumador(2, 0, 4)
                        End If
                    End If
                End If
            End If
        End If

        'Si no encontro ocasion para ganar buscamos obstaculizar al humano:
        If pensado = False Then
            For i As Integer = 0 To 2
                If filas(i) = 2 Then
                    pensado = True
                    'buscar hueco donde poner circulo en esa fila
                    For f As Integer = 0 To 2
                        If mLabels(i, f).Text = BLANCO Then
                            escribeYpasaTurno(i, f, "O")
                            sumador(i, f, 4)
                            Exit For
                        End If
                    Next
                    Exit For
                End If
            Next

            If pensado = False Then
                For i As Integer = 0 To 2
                    If colus(i) = 2 Then
                        pensado = True
                        'buscar hueco donde poner circulo en esa columna
                        For c As Integer = 0 To 2
                            If mLabels(c, i).Text = BLANCO Then
                                escribeYpasaTurno(c, i, "O")
                                sumador(c, i, 4)
                                Exit For
                            End If
                        Next
                        Exit For
                    End If
                Next
                


                If pensado = False Then
                    If diago(0) = 2 Then
                        pensado = True
                        'buscar hueco en la diagonal principal
                        If mLabels(0, 0).Text = BLANCO Then
                            escribeYpasaTurno(0, 0, "O")
                            sumador(0, 0, 4)
                        ElseIf mLabels(2, 2).Text = BLANCO Then
                            escribeYpasaTurno(2, 2, "O")
                            sumador(2, 2, 4)
                        End If
                    ElseIf diago(1) = 2 Then
                        pensado = True
                        'buscar hueco en la diagonal secundaria
                        If mLabels(0, 2).Text = BLANCO Then
                            escribeYpasaTurno(0, 2, "O")
                            sumador(0, 2, 4)
                        ElseIf mLabels(2, 0).Text = BLANCO Then
                            escribeYpasaTurno(2, 0, "O")
                            sumador(2, 0, 4)
                        End If
                    End If
                End If

                If pensado = False And turno = 3 Then
                    'Algoritmo anti René( jugada de la L)
                    If mLabels(0, 0).Text = "X" Or mLabels(0, 2).Text = "X" Or mLabels(2, 0).Text = "X" Or mLabels(2, 2).Text = "X" Then

                        If filas(0) = 4 Then
                            pensado = True
                            If mLabels(0, 2).Text = BLANCO Then
                                escribeYpasaTurno(0, 2, "O")
                                sumador(0, 2, 4)
                            Else
                                escribeYpasaTurno(0, 0, "O")
                                sumador(0, 0, 4)
                            End If
                        ElseIf filas(2) = 4 Then
                            pensado = True
                            If mLabels(2, 2).Text = BLANCO Then
                                escribeYpasaTurno(2, 2, "O")
                                sumador(2, 2, 4)
                            Else
                                escribeYpasaTurno(2, 0, "O")
                                sumador(2, 0, 4)
                            End If
                            'ElseIf colus(0) = 4 Then
                            '    escribeYpasaTurno(0, 1, "O")
                            '    sumador(0, 1, 4)
                            'ElseIf colus(2) = 4 Then
                            '    escribeYpasaTurno(2, 1, "O")
                            '    sumador(2, 1, 4)

                        ElseIf filas(1) = 4 Then
                            pensado = True
                            If mLabels(1, 0).Text = BLANCO Then
                                If colus(2) = 1 Then
                                    escribeYpasaTurno(1, 0, "O")
                                    sumador(1, 0, 4)
                                Else
                                    escribeYpasaTurno(1, 2, "O")
                                    sumador(1, 2, 4)
                                End If
                            Else
                                escribeYpasaTurno(1, 2, "O")
                                sumador(1, 2, 4)
                            End If

                        ElseIf colus(1) = 4 Then
                            pensado = True

                            If filas(2) = 1 Then
                                escribeYpasaTurno(0, 1, "O")
                                sumador(0, 1, 4)
                            Else
                                escribeYpasaTurno(2, 1, "O")
                                sumador(2, 1, 4)
                            End If


                        End If
                    Else
                        If filas(0) = 1 Then
                            pensado = True
                            If mLabels(0, 0).Text = BLANCO Then
                                escribeYpasaTurno(0, 0, "O")
                                sumador(0, 0, 4)
                            Else
                                escribeYpasaTurno(0, 2, "O")
                                sumador(0, 2, 4)
                            End If
                        ElseIf filas(2) = 1 Then
                            pensado = True
                            If mLabels(2, 0).Text = BLANCO Then
                                escribeYpasaTurno(2, 0, "O")
                                sumador(2, 0, 4)
                            Else
                                escribeYpasaTurno(2, 2, "O")
                                sumador(2, 2, 4)
                            End If
                        End If

                    End If

                End If

            End If
        End If

        Return pensado
    End Function

    Private Sub escribeYpasaTurno(ByVal i As Integer, ByVal j As Integer, ByVal simbolo As String)
        mLabels(i, j).Text = simbolo
        turno = turno + 1
        If turno = 9 Then gameOver = True
    End Sub



    'Pasamoslle indices para que nos devolve se está dentro do rango da matriz
    Private Function dentroMatriz(ByVal i As Integer, ByVal j As Integer) As Boolean
        If i < 0 Or i > 2 Or j < 0 Or j > 2 Then
            Return False
        Else : Return True
        End If
    End Function

    







    'Manexa o comportamento do botón saír
    Private Sub lblExit_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblExit.Click
        Application.Exit()
    End Sub
    'Manexa o comportamento do botón resetear
    Private Sub lblRes_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblRes.Click
        reseteo()
    End Sub

    

    'Métodos para manexar o sombreado das label según o mouse entra ou sae, dando un sombreado que resulta moi estético
    Private Sub lbl00_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl00.MouseEnter
        lbl00.BackColor = Color.DarkGray
    End Sub
    Private Sub lbl00_Exit(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl00.MouseLeave
        lbl00.BackColor = Color.DimGray
    End Sub

    Private Sub lbl01_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl01.MouseEnter
        lbl01.BackColor = Color.DarkGray
    End Sub
    Private Sub lbl01_Exit(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl01.MouseLeave
        lbl01.BackColor = Color.DimGray
    End Sub

    Private Sub lbl02_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl02.MouseEnter
        lbl02.BackColor = Color.DarkGray
    End Sub
    Private Sub lbl02_Exit(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl02.MouseLeave
        lbl02.BackColor = Color.DimGray
    End Sub

    Private Sub lbl10_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl10.MouseEnter
        lbl10.BackColor = Color.DarkGray
    End Sub
    Private Sub lbl10_Exit(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl10.MouseLeave
        lbl10.BackColor = Color.DimGray
    End Sub

    Private Sub lbl11_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl11.MouseEnter
        lbl11.BackColor = Color.DarkGray
    End Sub
    Private Sub lbl11_Exit(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl11.MouseLeave
        lbl11.BackColor = Color.DimGray
    End Sub

    Private Sub lbl12_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl12.MouseEnter
        lbl12.BackColor = Color.DarkGray
    End Sub
    Private Sub lbl12_Exit(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl12.MouseLeave
        lbl12.BackColor = Color.DimGray
    End Sub

    Private Sub lbl20_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl20.MouseEnter
        lbl20.BackColor = Color.DarkGray
    End Sub
    Private Sub lbl20_Exit(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl20.MouseLeave
        lbl20.BackColor = Color.DimGray
    End Sub

    Private Sub lbl21_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl21.MouseEnter
        lbl21.BackColor = Color.DarkGray
    End Sub
    Private Sub lbl21_Exit(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl21.MouseLeave
        lbl21.BackColor = Color.DimGray
    End Sub

    Private Sub lbl22_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl22.MouseEnter
        lbl22.BackColor = Color.DarkGray
    End Sub
    Private Sub lbl22_Exit(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbl22.MouseLeave
        lbl22.BackColor = Color.DimGray
    End Sub
    Private Sub lblExit_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblExit.MouseEnter
        lblExit.BackColor = Color.DarkGray
    End Sub
    Private Sub lblExit_Exit(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblExit.MouseLeave
        lblExit.BackColor = Color.DimGray
    End Sub
    Private Sub lblReset_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblRes.MouseEnter
        lblRes.BackColor = Color.DarkGray
    End Sub
    Private Sub lblReset_Exit(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblRes.MouseLeave
        lblRes.BackColor = Color.DimGray
    End Sub
    Private Sub lblFacil_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblFacil.MouseEnter
        lblFacil.BackColor = Color.DarkGray
    End Sub
    Private Sub lblFacil_Exit(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblFacil.MouseLeave
        lblFacil.BackColor = Color.WhiteSmoke
    End Sub
    Private Sub lblMedio_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblMedio.MouseEnter
        lblMedio.BackColor = Color.DarkGray
    End Sub
    Private Sub lblMedio_Exit(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblMedio.MouseLeave
        lblMedio.BackColor = Color.WhiteSmoke
    End Sub
    Private Sub lblDificil_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblDificil.MouseEnter
        lblDificil.BackColor = Color.DarkGray
    End Sub
    Private Sub lblDificil_Exit(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblDificil.MouseLeave
        lblDificil.BackColor = Color.WhiteSmoke
    End Sub




    

    
    Private Sub lblConfig_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblConfig.Click
        abrirMenu()
    End Sub

    Private Sub lblFacil_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblFacil.Click
        nivel = 1
        cerrarMenu()
        iniciaMaquina = False
        reseteo()
        reseteaPuntos()
    End Sub

    Private Sub lblMedio_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblMedio.Click
        nivel = 2
        cerrarMenu()
        iniciaMaquina = False
        reseteo()
        reseteaPuntos()
    End Sub

    Private Sub lblDificil_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lblDificil.Click
        nivel = 3
        cerrarMenu()
        iniciaMaquina = False
        reseteo()
        reseteaPuntos()

    End Sub

    Private Sub cerrarMenu()
        lblFondo.SendToBack()
        lblFacil.Visible = False
        lblMedio.Visible = False
        lblDificil.Visible = False
    End Sub

    Private Sub abrirMenu()
        lblFondo.BringToFront()
        lblFacil.Visible = True
        lblFacil.BringToFront()
        lblMedio.Visible = True
        lblMedio.BringToFront()
        lblDificil.Visible = True
        lblDificil.BringToFront()
    End Sub

    Private Sub reseteaPuntos()
        lblPunLose.Text = "0"
        lblPunWin.Text = "0"
    End Sub
End Class
