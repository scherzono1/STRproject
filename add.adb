
with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with Devices; use Devices;

-- Packages needed to generate pulse interrupts       
-- with Ada.Interrupts.Names;
-- with Pulse_Interrupt; use Pulse_Interrupt;

package body add is

    ----------------------------------------------------------------------
    ------------- procedure exported 
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;
    ----------------------------------------------------------------------

    task Cabeza is 
    pragma priority(4);
    end Cabeza;

    task Distancia is
    pragma priority(2);
    end Distancia;
    
    ---------------------------------------------------------
    -- Aqui se declaran las tareas que forman el STR
    ---------------------------------------------------------
    ----------------------------------------------------------------------
    ------------- declaracion del tipo de objeto protegido 
    ----------------------------------------------------------------------
    
    protected type Sintomas(valor_ini: integer) is 
    	pragma priority(4);

    	procedure escribirCabeza(nuevo_valor_bool: boolean);
    
    private 
    	dato: integer := valor_ini;
    	datoC: boolean := false;
    end Sintomas;
    
    protected body Medidas is
    	function leerVelocidad return Speed_Samples_Type is
    	begin
    		return velAct;
    	end leerVelocidad;
    	
    	function leerDistancia return Distance_Samples_Type is
    	begin
    		return distAct;
    	end leerDistancia;
    	
    	procedure escribir_dist_vel(dist: Distance_Samples_Type; vel: Speed_Samples_Type) is 
    	begin 
    	velAct := vel;
    	distAct := dist;
    	end escribir_dist_vel;
    end Medidas;

    sint: Sintomas(0);
    
    ----------------------------------------------------------------------
    ------------- cuerpo de objeto protegido 
    ---------------------------------------------------------------------- 
     
    protected body Sintomas is
    	procedure escribirCabeza(nuevo_valor_bool: boolean) is 
    	begin 
    	datoC := nuevo_valor_bool;
    	end escribirCabeza;
    end Sintomas;
    

    -----------------------------------------------------------------------
    ------------- body of tasks 
    -----------------------------------------------------------------------

    task body Cabeza is
    
    t_sig : Time;
    intervalo : Time_Span := To_Time_Span(0.4);
    cabeza_act: HeadPosition_Samples_Type := (0,0);
    cabeza_ant: HeadPosition_Samples_Type := (0,0);
    sw_act: Steering_Samples_Type := (0);
    sw_ant: Steering_Samples_Type := (0);
    
    t_start : Time;
    t_end : Time;
    
    begin
    t_sig := Big_Bang + intervalo;
    loop
    Starting_Notice("COMENZANDO TAREA CABEZA");
    
    t_start := Clock;
    
      Reading_HeadPosition (cabeza_act);
      Display_HeadPosition_Sample (cabeza_act);
      Reading_Steering (sw_act);
      Display_Steering (sw_act);

      IF(((cabeza_act(x) > 30) and (cabeza_ant(x) > 30)) OR
        ((cabeza_act(x) < -30) and (cabeza_ant(x) < -30)) OR
        (((cabeza_act(y) > 30) and (cabeza_ant(y) > 30)) AND ((sw_act < 5 and sw_ant < 5))) OR
        (((cabeza_act(y) < -30) and (cabeza_ant(y) < -30)) AND ((sw_act > -5 and sw_ant > -5))))

      THEN sint.escribirCabeza(true);

      ELSE sint.escribirCabeza(false);

      END IF;

      cabeza_ant := cabeza_act;
      sw_ant := sw_act;
      
      t_end := Clock;
      Put_Line("Tiempo total de la tarea Cabeza: "& Duration'Image(To_Duration(t_end - t_start)));
      Finishing_Notice("FIN TAREA CABEZA");
      delay until t_sig;
      t_sig := t_sig + intervalo;
    end loop;
    end Cabeza;


   task body Distancia is 
    
    dist_act: Distance_Samples_Type := 0;
    vel_act: Speed_Samples_Type := 0;
    sig_instante: Time;
    intervalo: Time_Span := To_Time_Span(0.3);
    cal_vel: Float := 0.0;
    
    begin
    sig_instante := Big_Bang + intervalo;
    loop
    	Starting_Notice("COMIENZA TAREA DISTANCIA");
      Reading_Distance (dist_act);
    	Reading_Speed (vel_act);
    	
    	medida.escribir_dist_vel(dist_act, vel_act);
    	
      cal_vel :=  Float((vel_act/10)**2);

    	if (Float(dist_act) < Float(cal_vel) )then
    		sint.escribir(DISTANCIA_INSEGURA);

    	elsif (Float(dist_act) < Float(cal_vel/2) )then
        	sint.escribir(DISTANCIA_IMPRUDENTE); 

      elsif(Float(dist_act) < Float(cal_vel/3) )then
        	sint.escribir(PELIGRO_COLISION);

      else  
         sint.escribir(DISTANCIA_SEGURA);
      
      end if;
      
      cal_vel := 0;

      Finishing_Notice("FIN TAREA DISTANCIA");
      delay until sig_instante;
      sig_instante := sig_instante + intervalo;
     end loop;
   end Distancia;



    ----------------------------------------------------------------------
    ------------- procedure para probar los dispositivos 
    ----------------------------------------------------------------------
    procedure Prueba_Dispositivos; 

    Procedure Prueba_Dispositivos is
        Current_V: Speed_Samples_Type := 0;
        Current_H: HeadPosition_Samples_Type := (+2,-2);
        Current_D: Distance_Samples_Type := 0;
        Current_O: Eyes_Samples_Type := (70,70);
        Current_E: EEG_Samples_Type := (1,1,1,1,1,1,1,1,1,1);
        Current_S: Steering_Samples_Type := 0;
    begin
         Starting_Notice ("Prueba_Dispositivo");

         for I in 1..120 loop
         -- Prueba distancia
            --Reading_Distance (Current_D);
            --Display_Distance (Current_D);
            --if (Current_D < 40) then Light (On); 
            --                    else Light (Off); end if;

         -- Prueba velocidad
            --Reading_Speed (Current_V);
            --Display_Speed (Current_V);
            --if (Current_V > 110) then Beep (2); end if;

         -- Prueba volante
         --   Reading_Steering (Current_S);
         --   Display_Steering (Current_S);
         --   if (Current_S > 30) OR (Current_S < -30) then Light (On);
         --                                            else Light (Off); end if;

         -- Prueba Posicion de la cabeza
            --Reading_HeadPosition (Current_H);
            --Display_HeadPosition_Sample (Current_H);
            --if (Current_H(x) > 30) then Beep (4); end if;

         -- Prueba ojos
            --Reading_EyesImage (Current_O);
            --Display_Eyes_Sample (Current_O);

         -- Prueba electroencefalograma
            --Reading_Sensors (Current_E);
            --Display_Electrodes_Sample (Current_E);
   
         delay until (Clock + To_time_Span(0.1));
         end loop;

         Finishing_Notice ("Prueba_Dispositivo");
    end Prueba_Dispositivos;


begin
   Starting_Notice ("Programa Principal");
   Prueba_Dispositivos;
   Finishing_Notice ("Programa Principal");
end add;



