with Lower_Layer_UDP, Chat_Messages, Handlers;
with Ada.Strings.Unbounded, Ada.Text_IO, Ada.Command_Line, Ada.Exceptions;

procedure Chat_Server_2 is

   package ATI renames           Ada.Text_IO;
   package LLU renames       Lower_Layer_UDP;
   package ASU renames Ada.Strings.Unbounded;
   package ACL renames      Ada.Command_Line;
   package CM  renames         Chat_Messages;

   Usage_Error : Exception;

   Usage : String := "./chat_server_2 <port><maxclients>";

   procedure Check_Command_Line is
   begin

      if ACL.Argument_Count /= 2 then
         raise Usage_Error;
      end if;

   end Check_Command_Line;

   procedure Create_Server (EP : in out LLU.End_Point_Type) is
      Host : ASU.Unbounded_String;
      Port, Max_Clients : Natural;
   begin

      Host := ASU.To_Unbounded_String(LLU.Get_Host_Name);
      Port := Natural'Value(ACL.Argument(1));
      Max_Clients := Natural'Value(ACL.Argument(2));
      Ada.Text_IO.Put_Line ("Host name: " & ASU.To_String(Host) & ", port:" & Natural'Image(Port));
      EP := LLU.Build (LLU.To_IP(ASU.To_String(Host)), Port);
      LLU.Bind (EP, Handlers.Server_Handler'Access);

   end Create_Server;

   Server_EP: LLU.End_Point_Type;
   C : Character;

begin

   Create_Server (Server_EP);

   loop

      ATI.Get_Immediate (C);

      if C = 'l' or C = 'L' then
         ATI.New_Line;
         ATI.Put_Line("   ACTIVE CLIENTS");
         ATI.Put_Line("=====================");
         Handlers.Print_Active_Clients;
         ATI.New_Line;
      elsif C = 'o' or C = 'O' then
         ATI.New_Line;
         ATI.Put_Line("     OLD CLIENTS");
         ATI.Put_Line("=====================");
         Handlers.Print_Old_Clients;
         ATI.New_Line;
      else
         ATI.Put_Line("No option for " & Character'Image(C));
      end if;

   end loop;

exception
   when Usage_Error =>
      ATI.Put_Line("Usage error: " & Usage);
      LLU.Finalize;
   when Constraint_Error =>
      ATI.Put_Line("Usage error: " & Usage);
      LLU.Finalize;
   when Ex:others =>
      Ada.Text_IO.Put_Line ("Unexpected exception: " &
                            Ada.Exceptions.Exception_Name(Ex) & " in: " &
                            Ada.Exceptions.Exception_Message(Ex));
      LLU.Finalize;

end Chat_Server_2;
