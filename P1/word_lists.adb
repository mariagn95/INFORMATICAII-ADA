with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Word_Lists is

	package ATI renames Ada.Text_IO;

	procedure Free is new Ada.Unchecked_Deallocation(Cell,Word_List_Type);


	procedure Add_Word (List: in out Word_List_Type;
						Word: in ASU.Unbounded_String) is  --List = primer puntero.
		P_Aux: Word_List_Type;
		In_List: Boolean := False; -- Booleano para ver si esta en la lista.

	begin
			if List = null then
				List := new Cell'(Word,1,null); -- creamos la lista desde el principio si no está creada.
			else
				P_Aux := List; --el puntero auxiliar apunta a List.
				while P_Aux /= null loop
					if ASU.To_String(Word) = ASU.To_String(P_Aux.Word) then  --guardamos cuantas veces se repite la palabra 
						P_Aux.Count := P_Aux.Count + 1;						-- si ya está en la lista. 
						In_List := True;
					end if;
					P_Aux := P_Aux.Next; -- vamos recorriendo la lista de un puntureo al siguiente.
				end loop;

				if not In_List then     -- si  no está en la lista vamos recorriendo la lista y haciando nuevas celdas.
					P_Aux:= List;

					while P_Aux.Next /= null loop
						P_Aux:= P_Aux.Next;
					end loop;
					P_Aux.Next:= new Cell'(Word,1,null);

				end if;
			
			end if;

	end Add_Word;


	procedure Delete_Word(List: in out Word_List_Type; Word: in ASU.Unbounded_String) is
		P_Anterior: Word_List_Type;
		P_Aux: Word_List_Type;
		Delete: Boolean := False;
	begin
		if List = null then
			raise Word_List_Error;
		end if;
		if ASU.To_String(List.Word) = ASU.To_String(Word) then -- comprobar primera palabra de la lista
			P_Aux := List;
			List := List.Next;
			Free(P_Aux);
			Delete := True;
		else -- comprobar resto de palabras de la lista
			P_Anterior := List;
			P_Aux := List.Next;
			while P_Aux /= null and not Delete loop
				if ASU.To_String(P_Aux.Word) = ASU.To_String(Word) then
					P_Anterior.Next := P_Aux.Next;
					Free(P_Aux);
					Delete := True;
				else
					P_Anterior := P_Aux;
					P_Aux := P_Aux.Next;
				end if;
			end loop;
		end if;
		if not Delete then
			raise Word_List_Error;
		end if;

	end Delete_Word;


	procedure Delete_List(List: in out Word_List_Type; Word: in ASU.Unbounded_String) is
		P_Aux: Word_List_Type;
	begin
		
		while List /= null loop
			P_Aux := List;
			List := List.Next;
			Free(P_Aux);
		end loop;
		Free(List); --liberamos la lista de palabras.

	end Delete_List;


	procedure Search_Word(List: in Word_List_Type; Word: in ASU.Unbounded_String;
						Count: out Natural) is
		P_Aux: Word_List_Type;
		Found: Boolean := False;
	begin
		P_Aux := List;
		while P_Aux /= null and not Found loop
			if ASU.To_String(P_Aux.Word) = ASU.To_String(Word) then
				Count := P_Aux.Count;
				Found := True;
			else
				P_Aux := P_Aux.Next;
			end if;
		end loop;

		if not Found then
			raise Word_List_Error;
		end if;
	end Search_Word;


	procedure Max_Word (List: in Word_List_Type; Word: out ASU.Unbounded_String;
						Count: out Natural) is
		P_Aux: Word_List_Type;
	begin
		if List /= null then
			Word := List.Word;
			Count := List.Count;
			P_Aux := List.Next;
			while P_Aux /= null loop
				if P_Aux.Count > Count then
					Count := P_Aux.Count;
					Word := P_Aux.Word;
				end if;
					P_Aux := P_Aux.Next;
			end loop;
		else
			raise Word_List_Error;
		end if;


	end Max_Word; 


	procedure Print_All(List: in Word_List_Type) is  --nos imprime las palabras que hay en el el texto y las veces
		P_Aux: Word_List_Type;						 -- que se repiten.
	begin
		if List = null then
			raise Word_List_Error;
		else
			P_Aux := List;
			while P_Aux /= null loop
				ATI.Put_Line("|" & ASU.To_String(P_Aux.Word) & "| - " & Natural'Image(P_Aux.Count));
				P_Aux := P_Aux.Next;
			end loop;
		end if;
	end Print_All;


end Word_Lists;
