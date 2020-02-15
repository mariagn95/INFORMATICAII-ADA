with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Characters.Handling;
with Ada.Strings.Maps;
with Word_Lists;


procedure Words is

	package ATI renames Ada.Text_IO;
	package ASU renames Ada.Strings.Unbounded;
	package ACL renames Ada.Command_Line;
	package ACH renames Ada.Characters.Handling;

	Usage_Error: Exception;

	procedure Separate_Words(P_List: in out Word_Lists.Word_List_Type; Line: in out ASU.Unbounded_String) is
		Word: ASU.Unbounded_String;
		Characters: Natural;
		Finish: Boolean := False;
	begin

		while not Finish loop
			Characters := ASU.Index(Line, Ada.Strings.Maps.To_Set(" ñ{},.;:_-(ºª\/|@#~$%=')><?!" & Character'Val(9)));
			if Characters /= 0 then
				Word := ASU.Head(Line,Characters-1);
				ASU.Tail(Line,ASU.Length(Line)-Characters);
			else
				Word := Line;
				Finish := True;
			end if;

			if ASU.To_String(Word) /= "" then
				Word_Lists.Add_Word(P_List,Word);
			end if;
			
		end loop;

	end Separate_Words;

	procedure Create_List(List: in out Word_Lists.Word_List_Type) is
		File: ATI.File_Type;
		Line: ASU.Unbounded_String;
		File_Name: ASU.Unbounded_String;
	begin
		if ACL.Argument(1) = "-i" then
			if ACL.Argument_Count = 2 then
				File_Name := ASU.To_Unbounded_String(ACL.Argument(2));
			else
				raise Usage_Error; -- cuando escribimos mal el comando
			end if;
		else
			if ACL.Argument_Count = 1 then
				File_Name := ASU.To_Unbounded_String(ACL.Argument(1));
			else
				raise Usage_Error;
			end if;
		end if;
		begin
			ATI.Open(File,ATI.In_File,ASU.To_String(File_Name)); --abrir el fichero
			while not ATI.End_Of_File(File) loop -- mientras no sea el final del fichero
				Line:= ASU.To_Unbounded_String(ATI.Get_Line(File)); -- lo leo
				Line := ASU.To_Unbounded_String(ACH.To_Lower(ASU.To_String(Line)));-- lo convierto a minusculas
				Separate_Words(List,Line);-- lo guardo
			end loop;
			ATI.Close(File);
		exception
			when ADA.IO_EXCEPTIONS.NAME_ERROR =>
				ATI.Put_Line("File not found");
		end;

	end Create_List;

	procedure Start_Menu(List: in out Word_Lists.Word_List_Type) is
		Word: ASU.Unbounded_String;
		Option: Integer := 0;
		File_Name: ASU.Unbounded_String;
		Count: Natural;
	begin
	
		if ACL.Argument(1) = "-i" then
			while Option /= 5 loop
				ATI.Put_Line("Options:");
				ATI.Put_Line("1 Add Word");
				ATI.Put_Line("2 Delete word");
				ATI.Put_Line("3 Search word");
				ATI.Put_Line("4 Show all words");
				ATI.Put_Line("5 Quit");
				ATI.Put("Your option? ");
				begin
					Option:= Integer'Value(ATI.Get_Line);

					case Option is
					when 1 =>
						ATI.Put("Word? ");
						Word := ASU.To_Unbounded_String(ATI.Get_Line);
						Word := ASU.To_Unbounded_String(ACH.To_Lower(ASU.To_String(Word)));
						ATI.Put_Line("Word |" & ASU.To_String(Word) & "| added");
						Word_Lists.Add_Word(List,Word);
					
					when 2 =>
						ATI.Put("Word? ");
						Word := ASU.To_Unbounded_String(ATI.Get_Line);
						Word_Lists.Delete_Word(List,Word);
						ATI.Put_Line("Word |" & ASU.To_String(Word) & "| deleted");
						
					when 3 =>
						ATI.Put("Word? ");
						Word := ASU.To_Unbounded_String(ATI.Get_Line);
						Word := ASU.To_Unbounded_String(ACH.To_Lower(ASU.To_String(Word)));
						Word_Lists.Search_Word(List,Word,Count);
						ATI.Put_Line("|" & ASU.To_String(Word) & "| - " &
									Natural'Image(Count));
					when 4 =>
						Word_Lists.Print_All(List);
					when 5 =>
						Word_Lists.Max_Word(List,Word,Count);
						ATI.Put_Line("Most frequent word: |" & ASU.To_String(Word) &
									"| - " & Natural'Image(Count));
						Word_Lists.Delete_List(List,Word);

					when others =>
						ATI.Put_Line("Incorrect option. Retry");
					end case;
				exception
					when Constraint_Error =>
						ATI.Put_Line("Incorrect option. Retry");
					when Word_Lists.Word_List_Error =>
						ATI.Put_Line("No words");
				end;
				ATI.New_Line(2);
			end loop;

		else
			Word_Lists.Max_Word(List,Word,Count);
			ATI.Put_Line("Most frequent word: |" & ASU.To_String(Word) &
							"| - " & Natural'Image(Count));
		end if;

	end Start_Menu;
	
	List: Word_Lists.Word_List_Type;

begin

	Create_List(List);
	Start_Menu(List);
	exception
		when Usage_Error =>
			ATI.Put_Line("Usage error: words [-i] <namefile>");
	
end Words;	
