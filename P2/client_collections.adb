with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Client_Collections is

   package ATI renames Ada.Text_IO;

   procedure Free is new Ada.Unchecked_Deallocation(Cell,Cell_A);


   procedure Add_Client (Collection: in out Collection_Type;
                         EP		   : in LLU.End_Point_Type;
                         Nick	   : in ASU.Unbounded_String;
                         Unique	   : in Boolean) is
   
      P_Aux: Cell_A; --Apunta a List.
	  In_Collection: Boolean := False; -- Booleano para ver si esta en la lista.
   begin
      if Collection.P_First = null then
			Collection.P_First := new Cell'(EP,Nick,null); -- creamos la lista desde el principio si no está creada.
         Collection.Total := 1; --cuenta de todos los clientes.
      else
			P_Aux := Collection.P_First; --el puntero auxiliar apunta a List.
			while P_Aux /= null and not In_Collection loop
				if ASU.To_String(Nick) = ASU.To_String(P_Aux.Nick) and Unique then
					In_Collection := True;
				else
				   P_Aux := P_Aux.Next; -- vamos recorriendo la lista de un puntero al siguiente.
            end if;
			end loop;

			if not In_Collection then -- si  no está en la lista vamos recorriendo la lista y haciando nuevas celdas.
            P_Aux := new Cell'(EP,Nick,Collection.P_First);
            Collection.P_First := P_Aux;
            Collection.Total := Collection.Total + 1;
         else
             raise Client_Collection_Error;
			end if;
			
		end if;
   end Add_Client;

   procedure Delete_Client (Collection: in out Collection_Type;
                            Nick	  : in ASU.Unbounded_String) is
      P_Aux: Cell_A;
      P_Anterior: Cell_A;
      Delete: Boolean := False;
   begin
      if Collection.P_First /= null then
         if ASU.To_String(Collection.P_First.Nick) = ASU.To_String(Nick) then
            Delete := True;
            P_Aux := Collection.P_First;
            Collection.P_First := Collection.P_First.Next;
            Free(P_Aux);
            Collection.Total := Collection.Total - 1;
         else
            P_Anterior := Collection.P_First;
            P_Aux := Collection.P_First.Next;
            while P_Aux /= null and not Delete loop
               if ASU.To_String(P_Aux.Nick) = ASU.To_String(Nick) then
                  P_Anterior.Next := P_Aux.Next;
                  Free(P_Aux);
                  Delete := True;
                  Collection.Total := Collection.Total - 1;
               else
                  P_Anterior := P_Aux;
                  P_Aux := P_Aux.Next;
               end if;
            end loop;
         end if;
      end if;
      
      if not Delete then
         raise Client_Collection_Error;
		end if;

   end Delete_Client;

   function Search_Client (Collection: in Collection_Type;
                           EP		 : in LLU.End_Point_Type)
                           return ASU.Unbounded_String is
      P_Aux: Cell_A := Collection.P_First;
      Found: Boolean := False;
      Nick: ASU.Unbounded_String;
   begin
      while P_Aux /= null and not Found loop
         if LLU.Image(EP) = LLU.Image(P_Aux.Client_EP) then
            Found := True;
            Nick := P_Aux.Nick;
         else
            P_Aux := P_Aux.Next;
         end if;
      end loop;
      if Found then
         return Nick;
      else
         raise Client_Collection_Error;
      end if;
   end Search_Client;

   procedure Send_To_All (Collection: in Collection_Type;
                          P_Buffer	: access LLU.Buffer_Type) is
      P_Aux: Cell_A := Collection.P_First;
   begin
      while P_Aux /= null loop
         LLU.Send(P_Aux.Client_EP,P_Buffer);
         P_Aux := P_Aux.Next;
      end loop;
   end Send_To_All;

   function Collection_Image (Collection: in Collection_Type)
                             return String is
      P_Aux: Cell_A := Collection.P_First;
      IP, Port: ASU.Unbounded_String;
      Client: ASU.Unbounded_String;
      Image: ASU.Unbounded_String;
   begin
      Image := ASU.To_Unbounded_String("");
      while P_Aux /= null loop
         Client := ASU.To_Unbounded_String(LLU.Image(P_Aux.Client_EP));
         IP := ASU.Tail(Client,ASU.Length(Client)-ASU.Index(Client,":"));
         Port := ASU.Tail(IP,ASU.Length(IP)-ASU.Index(IP,":")-2);
         ASU.Head(IP,ASU.Index(IP,",")-1);
         Image := ASU.To_Unbounded_String(ASU.To_String(Image) & ASU.To_String(IP) & ":" & ASU.To_String(Port) & " " &
                                          ASU.To_String(P_Aux.Nick));
         Image := ASU.To_Unbounded_String(ASU.To_String(Image) & ASCII.LF);
         P_Aux := P_Aux.Next;
      end loop;

      return ASU.To_String(Image);
   end Collection_Image;

end Client_Collections;
