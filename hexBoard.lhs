> -- |
> -- hexBoard.hs - A program for drawing boards for the game of Hex.
> -- Copyright (C) 2015  Jesse Huard <jhuard@ualberta.ca>
> --
> -- This program is free software: you can redistribute it and/or modify
> -- it under the terms of the GNU General Public License as published by
> -- the Free Software Foundation, either version 3 of the License, or
> -- (at your option) any later version.
> --
> -- This program is distributed in the hope that it will be useful,
> -- but WITHOUT ANY WARRANTY; without even the implied warranty of
> -- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
> -- GNU General Public License for more details.
> --
> -- You should have received a copy of the GNU General Public License
> -- along with this program.  If not, see <http://www.gnu.org/licenses/>.
> {-# LANGUAGE NoMonomorphismRestriction #-}
>
> import Diagrams.Prelude
> import Diagrams.TwoD.Vector
> import Diagrams.Backend.SVG.CmdLine
>
> hexRow n = cat (1 *^ e (pi/6 @@ rad)) (replicate n (hexagon 1))
> placeTogether a b = a # snugT <> b # snugB
> hexGrid n = foldl placeTogether (hexRow n) (replicate (n - 1) $ hexRow n)
>
> main = mainWith (hexGrid 19 :: Diagram B)
