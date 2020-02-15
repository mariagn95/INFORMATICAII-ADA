with Ada.Text_IO, Ada.Command_Line, Ada.Strings.Unbounded, Ada.Calendar, Gnat.Calendar.Time_IO;
with Chat_Messages, Maps_G;

package body Handlers is
   
   package ASU renames Ada.Strings.Unbounded;
   package ATI renames           Ada.Text_IO;
   package ACL renames      Ada.Command_Line;
   package AC  renames          Ada.Calendar;
   package GCT renames Gnat.Calendar.Time_IO;
   package CM  renames         Chat_Messages;

   use type CM.Message_Type;
   use type Ada.Calendar.Time;

   type Client_Type is record
      Handler :   LLU.End_Point_Type;
      Receive :   LLU.End_Point_Type;
      Nick    : ASU.Unbounded_String;
   end record;

   type Value_Type is record
      EP   :   LLU.End_Point_Type;
      Time :              AC.Time;
   end record;

   function Max_Clients return Natural is
   begin

      return Natural'Value(ACL.Argument(2));

   exception
      when Ex:others =>
         return 0;
   end Max_Clients;

   package Active is new Maps_G (ASU.Unbounded_String, Value_Type, ASU."=", Max_Clients);
   package Old    is new Maps_G (ASU.Unbounded_String, ASU.Unbounded_String, ASU."=");

   Active_Clients : Active.Map;
   Old_Clients    : Old.Map;

   Init_Message   : String := " joins the chat";
   Logout_Message : String := " leaves the chat";
   Ban_Message    : String := " banned for being idle too long";

   procedure Client_Handler (From     :     in LLU.End_Point_Type;
                             To       :     in LLU.End_Point_Type;
                             P_Buffer : access    LLU.Buffer_Type) is
      Mess          :      CM.Message_Type;
      Nick, Request : ASU.Unbounded_String;
   begin

      Mess := CM.Message_Type'Input(P_Buffer);

      if Mess = CM.Server then
         Nick := ASU.Unbounded_String'Input (P_Buffer);
         Request := ASU.Unbounded_String'Input (P_Buffer);
         ATI.New_Line;
         ATI.Put_Line(ASU.To_String(Nick) & ": " & ASU.To_String(Request));
         ATI.Put(">> ");
      end if;

      LLU.Reset (P_Buffer.all);

   end Client_Handler;

   function Time_Image (Time : AC.Time) return String is
   begin
      return GCT.Image (Time, "%d-%b-%y %T.%i");
   end Time_Image;

   procedure Send_Welcome (Client   : in   LLU.End_Point_Type;
                           Success  : in              Boolean;
                           P_Buffer : access  LLU.Buffer_Type) is
   begin

      CM.Message_Type'Output(P_Buffer, CM.Welcome);
      Boolean'Output (P_Buffer, Success);

      LLU.Send (Client, P_Buffer);

   end Send_Welcome;

   procedure Send_Server (Active_List : in out       Active.Map;
                          Client_EP   : in   LLU.End_Point_Type;
                          P_Buffer    : access  LLU.Buffer_Type) is
      Cursor :       Active.Cursor := Active.First (Active_List);
      E      : Active.Element_Type;
   begin

      loop

         E := Active.Element (Cursor);

         if LLU.Image (E.Value.EP) /= LLU.Image (Client_EP) then
            LLU.Send (E.Value.EP, P_Buffer);
         end if;

         Active.Next (Cursor);

      end loop;

   exception
      when Active.No_Element =>
         null;

   end Send_Server;

   function Get_IP (EP : LLU.End_Point_Type) return String is
        IP, EP_Image : ASU.Unbounded_String;
    begin

        EP_Image := ASU.To_Unbounded_String (LLU.Image (EP));
        IP       := ASU.Head (EP_Image, ASU.Index (EP_Image, ",") - 1);

        ASU.Tail (IP, ASU.Length (IP) - ASU.Index (IP, ":") - 1);

        return ASU.To_String(IP);

    end Get_IP;

    function Get_Port (EP : LLU.End_Point_Type) return String is
        Port, EP_Image : ASU.Unbounded_String;
    begin

        EP_Image := ASU.To_Unbounded_String (LLU.Image (EP));
        Port     := ASU.Tail (EP_Image, ASU.Length (EP_Image) - ASU.Index (EP_Image,","));

        ASU.Tail (Port, ASU.Length (Port) - ASU.Index (Port, ":") - 2);

        return ASU.To_String(Port);

    end Get_Port;

   procedure Print_Active_Clients is
      Cursor   :       Active.Cursor := Active.First (Active_Clients);
      E        : Active.Element_Type;
      EP_Image : ASU.Unbounded_String;
   begin

      loop

         E := Active.Element (Cursor);

         ATI.Put_Line(ASU.To_String(E.Key) & " (" & Get_IP(E.Value.EP) & ":" & Get_Port(E.Value.EP) & "): " &
                      Time_Image(E.Value.Time));

         Active.Next (Cursor);

      end loop;

   exception
      when Active.No_Element =>
         null;

   end Print_Active_Clients;

   procedure Print_Old_Clients is
      Cursor :       Old.Cursor := Old.First (Old_Clients);
      E      : Old.Element_Type;
   begin

      loop

         E := Old.Element (Cursor);

         ATI.Put_Line(ASU.To_String(E.Key) & ": " & ASU.To_String(E.Value));

         Old.Next (Cursor);

      end loop;

   exception
      when Old.No_Element =>
         null;

   end Print_Old_Clients;

   procedure Prepare_Server (Message, Nick : in ASU.Unbounded_String;
                             P_Buffer      : access  LLU.Buffer_Type) is
   begin

      CM.Message_Type'Output (P_Buffer, CM.Server);
      ASU.Unbounded_String'Output(P_Buffer, Nick);
      ASU.Unbounded_String'Output(P_Buffer, Message);

   end Prepare_Server;

   procedure Ban (Active_List : in out      Active.Map;
                  Old_List    : in out         Old.Map;
                  New_Client  : in         Client_Type;
                  P_Buffer    : access LLU.Buffer_Type) is
      Cursor        :        Active.Cursor := Active.First (Active_List);
      E_Aux, E_Old  :  Active.Element_Type;
      Success       :              Boolean := False;
      Time, Message : ASU.Unbounded_String;
      Value         :           Value_Type;
   begin

      E_Old := Active.Element (Cursor);
      Active.Next (Cursor);

      loop

         E_Aux := Active.Element (Cursor);

         if AC."<"(E_Aux.Value.Time, E_Old.Value.Time) then
            E_Old := E_Aux;
         end if;

         Active.Next (Cursor);

      end loop;

   exception
      when Active.No_Element =>

         Active.Delete (Active_List, E_Old.Key, Success);
         Time := ASU.To_Unbounded_String (Time_Image(E_Old.Value.Time));
         Old.Put (Old_List, E_Old.Key, Time);
         LLU.Reset (P_Buffer.all);
         Message := ASU.To_Unbounded_String(ASU.To_String(E_Old.Key) & Ban_Message);
         Prepare_Server (Message, ASU.To_Unbounded_String("server"), P_Buffer);
         Send_Server (Active_List, E_Old.Value.EP, P_Buffer);
         LLU.Send (E_Old.Value.EP, P_Buffer);

         Value.EP := New_Client.Handler;
         Value.Time := AC.Clock;
         Active.Put (Active_List, New_Client.Nick, Value);
         ATI.Put_Line(" ACCEPTED");
         LLU.Reset (P_Buffer.all);
         Message := ASU.To_Unbounded_String(ASU.To_String(New_Client.Nick) & Init_Message);
         Prepare_Server (Message, ASU.To_Unbounded_String("server"), P_Buffer);
         Send_Server (Active_List, New_Client.Handler, P_Buffer);
         LLU.Reset (P_Buffer.all);
         Send_Welcome (New_Client.Receive, True, P_Buffer);

   end Ban;

   procedure Receive_Init (Active_List : in out      Active.Map;
                           Old_List    : in out         Old.Map;
                           P_Buffer    : access LLU.Buffer_Type) is
      Client  : Client_Type;
      Value   :  Value_Type;
      Success :     Boolean := False;
      Message : ASU.Unbounded_String;
   begin

      Client.Receive := LLU.End_Point_Type'Input (P_Buffer);
      Client.Handler := LLU.End_Point_Type'Input (P_Buffer);
      Client.Nick    := ASU.Unbounded_String'Input (P_Buffer);

      ATI.Put(ASU.To_String(Client.Nick));

      Active.Get (Active_List, Client.Nick, Value, Success);

      if not Success then
         Value.EP := Client.Handler;
         Value.Time := AC.Clock;
         Active.Put (Active_List, Client.Nick, Value);
         ATI.Put_Line(" ACCEPTED");
         LLU.Reset (P_Buffer.all);
         Message := ASU.To_Unbounded_String(ASU.To_String(Client.Nick) & Init_Message);
         Prepare_Server (Message, ASU.To_Unbounded_String("server"), P_Buffer);
         Send_Server (Active_List, Client.Handler, P_Buffer);
      else
         ATI.Put_Line(" IGNORED, nick already used");
      end if;

      LLU.Reset (P_Buffer.all);

      Send_Welcome (Client.Receive, not Success, P_Buffer);

   exception
      when Handlers.Active.Full_Map =>
         Ban (Active_List, Old_List, Client, P_Buffer);

   end Receive_Init;

   procedure Receive_Writer (Active_List : in out Active.Map;
                             P_Buffer : access LLU.Buffer_Type) is
      Client  :          Client_Type;
      Value   :           Value_Type;
      Request : ASU.Unbounded_String;
      Success :              Boolean := False;
      Message : ASU.Unbounded_String;
   begin

      Client.Handler := LLU.End_Point_Type'Input (P_Buffer);
      Client.Nick    := ASU.Unbounded_String'Input (P_Buffer);
      Request        := ASU.Unbounded_String'Input (P_Buffer);

      Active.Get (Active_Clients, Client.Nick, Value, Success);

      if not Success then
         ATI.Put_Line("unknown client, IGNORED");
      else
         Value.EP := Client.Handler;
         Value.Time := AC.Clock;
         Active.Put (Active_Clients, Client.Nick, Value);
         LLU.Reset (P_Buffer.all);
         ATI.Put_Line (ASU.To_String(Client.Nick) & ": " & ASU.To_String(Request));
         Prepare_Server (Request, Client.Nick, P_Buffer);
         Send_Server (Active_List, Client.Handler, P_Buffer);
      end if;

   end Receive_Writer;

   procedure Receive_Logout (Active_List : in out Active.Map;
                             Old_List : in out Old.Map;                             
                             P_Buffer : access LLU.Buffer_Type) is
      Client  :          Client_Type;
      Value   :           Value_Type;
      Time    : ASU.Unbounded_String;
      Success :              Boolean := False;
      Message : ASU.Unbounded_String;
   begin

      Client.Handler := LLU.End_Point_Type'Input (P_Buffer);
      Client.Nick    := ASU.Unbounded_String'Input (P_Buffer);

      Active.Get (Active_List, Client.Nick, Value, Success);

      if not Success then
         ATI.Put_Line("unknown client, IGNORED");
      else
         Time := ASU.To_Unbounded_String(Time_Image(AC.Clock));
         Active.Delete (Active_List, Client.Nick, Success);
         Old.Put (Old_List, Client.Nick, Time);
         ATI.Put_Line (ASU.To_String(Client.Nick));

         LLU.Reset (P_Buffer.all);
         Message := ASU.To_Unbounded_String(ASU.To_String (Client.Nick) & Logout_Message);
         Prepare_Server (Message, ASU.To_Unbounded_String("server"), P_Buffer);
         Send_Server (Active_List, Client.Handler, P_Buffer);
      end if;

   end Receive_Logout;

   procedure Server_Handler (From     :     in LLU.End_Point_Type;
                             To       :     in LLU.End_Point_Type;
                             P_Buffer : access    LLU.Buffer_Type) is
      Mess : CM.Message_Type;
   begin

      Mess := CM.Message_Type'Input (P_Buffer);

      ATI.Put (CM.Message_Type'Image(Mess) & " received from ");

      if Mess = CM.Init then
         Receive_Init (Active_Clients, Old_Clients, P_Buffer);
      elsif Mess = CM.Writer then
         Receive_Writer (Active_Clients, P_Buffer);
      elsif Mess = CM.Logout then
          Receive_Logout (Active_Clients, Old_Clients, P_Buffer);
      end if;

      LLU.Reset (P_Buffer.all);

   end Server_Handler;

end Handlers;
