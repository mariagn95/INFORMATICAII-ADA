with Ada.Strings.Unbounded, Ada.Text_IO, Ada.Command_Line, Ada.Exceptions;
with Lower_Layer_UDP, Chat_Messages, Handlers;

procedure Chat_Client_2 is

   package ATI renames           Ada.Text_IO;
   package LLU renames       Lower_Layer_UDP;
   package ASU renames Ada.Strings.Unbounded;
   package ACL renames      Ada.Command_Line;
   package CM  renames         Chat_Messages;

   use type CM.Message_Type;

   Server_Unreachable, Nick_In_Use, Usage_Error : Exception;

   Usage : String := "./chat_client_2 <serverhost><serverport><nickname>";

   type Client_Type is record
      Handler :   LLU.End_Point_Type;
      Receive :   LLU.End_Point_Type;
      Nick    : ASU.Unbounded_String;
   end record;

   procedure Check_Command_Line is
   begin

      if ACL.Argument_Count /= 3 then
         raise Usage_Error;
      end if;

   end Check_Command_Line;

   procedure Send_Init (Client    : in         Client_Type;
                        Server_EP : in  LLU.End_Point_Type;
                        P_Buffer  : access LLU.Buffer_Type) is
   begin

      CM.Message_Type'Output (P_Buffer, CM.Init);
      LLU.End_Point_Type'Output (P_Buffer, Client.Receive);
      LLU.End_Point_Type'Output (P_Buffer, Client.Handler);
      ASU.Unbounded_String'Output (P_Buffer, Client.Nick);

      LLU.Send (Server_EP, P_Buffer);

   end Send_Init;

   procedure Create_Client (Client : out Client_Type) is
      Receive, Handler : LLU.End_Point_Type;
   begin

      Client.Nick := ASU.To_Unbounded_String(ACL.Argument(3));

      LLU.Bind_Any (Receive);
      LLU.Bind_Any (Handler, Handlers.Client_Handler'Access);

      Client.Receive := Receive;
      Client.Handler := Handler;

   end Create_Client;

   procedure Create_Server (EP : out LLU.End_Point_Type) is
      Port    :              Natural;
      Maquina : ASU.Unbounded_String;
   begin

      Maquina := ASU.To_Unbounded_String (ACL.Argument(1));
      Port    := Natural'Value (ACL.Argument(2));
      EP      := LLU.Build (LLU.To_IP(ASU.To_String(Maquina)), Port);

   end Create_Server;

   procedure Receive_Welcome (Client   : in         Client_Type;
                              P_Buffer : access LLU.Buffer_Type) is
      Mess              : CM.Message_Type;
      Expired, Accepted :         Boolean := False;
   begin

      LLU.Receive (Client.Receive, P_Buffer, 10.0, Expired);

      if not Expired then
         ATI.Put("Mini-Chat v2.0: ");
         Mess := CM.Message_Type'Input (P_Buffer);
         if Mess = CM.Welcome then
            Accepted := Boolean'Input (P_Buffer);
            if not Accepted then
               raise Nick_In_Use;
            else
               ATI.Put_Line("Welcome " & ASU.To_String(Client.Nick));
            end if;
         else
            null;
         end if;
      else
         raise Server_Unreachable;
      end if;

   end Receive_Welcome;

   procedure Send_Writer (Client    : in          Client_Type;
                          Request   : in ASU.Unbounded_String;
                          Server_EP : in   LLU.End_Point_Type;
                          P_Buffer  : access  LLU.Buffer_Type) is
   begin

      CM.Message_Type'Output (P_Buffer, CM.Writer);
      LLU.End_Point_Type'Output (P_Buffer, Client.Handler);
      ASU.Unbounded_String'Output (P_Buffer, Client.Nick);
      ASU.Unbounded_String'Output (P_Buffer, Request);

      LLU.Send (Server_EP, P_Buffer);

   end Send_Writer;

   procedure Send_Logout (Client    : in          Client_Type;
                          Server_EP : in   LLU.End_Point_Type;
                          P_Buffer  : access  LLU.Buffer_Type) is
   begin

      CM.Message_Type'Output (P_Buffer, CM.Logout);
      LLU.End_Point_Type'Output (P_Buffer, Client.Handler);
      ASU.Unbounded_String'Output (P_Buffer, Client.Nick);

      LLU.Send (Server_EP, P_Buffer);

   end Send_Logout;

   Server_EP : LLU.End_Point_Type;
   Client : Client_Type;
   Buffer : aliased LLU.Buffer_Type(1024);
   Request, Reply : ASU.Unbounded_String;

begin

   Create_Server (Server_EP);

   Create_Client (Client);

   LLU.Reset (Buffer);

   Send_Init (Client, Server_EP, Buffer'Access);

   LLU.Reset (Buffer);

   Receive_Welcome (Client, Buffer'Access);

   LLU.Reset (Buffer);

   loop

      ATI.Put(">> ");
      Request := ASU.To_Unbounded_String(ATI.Get_Line);

      if ASU.To_String(Request) = ".quit" then
         ATI.New_Line;
         ATI.Put_Line("Bye!");
         Send_Logout (Client, Server_EP, Buffer'Access);
         exit;
      end if;

      Send_Writer (Client, Request, Server_EP, Buffer'Access);

      LLU.Reset (Buffer);

   end loop;

   LLU.Finalize;

exception
   when Usage_Error =>
      ATI.Put_Line("Usage error: " & Usage);
      LLU.Finalize;
   when Constraint_Error =>
      ATI.Put_Line("Usage error: " & Usage);
      LLU.Finalize;
   when Nick_In_Use =>
      ATI.Put_Line("IGNORED new user " & ASU.To_String (Client.Nick) & ", nick already used.");
      LLU.Finalize;
   when Server_Unreachable =>
      ATI.Put_Line("Server unreachable.");
      LLU.Finalize;
   when Ex:others =>
      ATI.Put_Line ("Unexpected exception: " &
                    Ada.Exceptions.Exception_Name(Ex) & " in: " &
                    Ada.Exceptions.Exception_Message(Ex));
      LLU.Finalize;

end Chat_Client_2;
