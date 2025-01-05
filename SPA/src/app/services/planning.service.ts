import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { map, Observable, tap } from 'rxjs';

export interface SurgeryRoom {
  roomNumber: string;
}

// Representa uma tupla no agOpRoomBetter



export interface Schedule {
  end: number; // Tempo de término
  operationId: string; // ID da operação
  start: number; // Tempo de início
}

// Interface para a sala e seus schedules
export interface RoomSchedule {
  room: string; // Nome da sala
  schedule: Schedule[]; // Lista de schedules
}

// Interface para o objeto principal
export interface ScheduleData {
  schedules: RoomSchedule[]; // Lista de RoomSchedule
  date: number; // Data (YYYYMMDD)
}


@Injectable({
  providedIn: 'root',
})
export class PlanningService {
  
  private planningUrl = 'http://10.9.22.225:2224/planning'; // URL base para o módulo de planeamento

  constructor(private http: HttpClient) {}

  greet(name: string) {
    let params = new HttpParams();

    if (name) {
      params = params.append('name', name);
    }

    return this.http.get(`${this.planningUrl}/greet`, { params });
  }


  geneticAlgorithm(
    populationSize: number, 
    generations: number, 
    mutationRate: number, 
    crossoverRate: number, 
    bestIndividualToBeKeptRate: number, 
    lowerCostWanted: number, 
    timeLimit: number, 
    date: string
  ) {
    let params = new HttpParams();
  
  
    const formattedDate = date.replace(/-/g, '');
  
    params = params
      .append('populationSize', populationSize.toString())
      .append('generations', generations.toString())
      .append('mutationRate', mutationRate.toString())
      .append('crossoverRate', crossoverRate.toString())
      .append('bestIndividualsToBeKeptRate', bestIndividualToBeKeptRate.toString())
      .append('lowerCostWanted', lowerCostWanted.toString())
      .append('timeLimit', timeLimit.toString())
      .append('date', formattedDate);
  
    // Interceptando e limpando a resposta
    return this.http.get(`${this.planningUrl}/getGeneticAlgorithmSchedule`, { responseType: 'text', params })
      .pipe(
        map((response: string) => {
          // Remover metadados da resposta e parsear como JSON
          const jsonStartIndex = response.indexOf('{');
          const jsonEndIndex = response.lastIndexOf('}');
          if (jsonStartIndex === -1 || jsonEndIndex === -1) {
            throw new Error('Invalid JSON response from server.');
          }
  
          const cleanJson = response.substring(jsonStartIndex, jsonEndIndex + 1);
          console.log('Clean JSON:', cleanJson);
  
          return JSON.parse(cleanJson) as ScheduleData;
        }),
        tap((parsedResponse) => {
          console.log('Parsed Response:', parsedResponse);
        })
      );
  }
  

  getSurgeryRooms(): Observable<SurgeryRoom[]> {
    return this.http.get<SurgeryRoom[]>('/api/OperationRoom/GetAll');
  }

  getScheduleFromPlanning(date: string, roomNumber: string): Observable<Schedule> {
    let params = new HttpParams();
    params = params.append('day', date);
    params = params.append('room', roomNumber);
    return this.http.get<Schedule>(`${this.planningUrl}/getSchedule`, { params });
  }
 


getHeuristicScheduleFromPlanning(date: string, roomNumber: string): Observable<Schedule> {
      let params = new HttpParams();
      params = params.append('day', date);
      params = params.append('room', roomNumber);
      return this.http.get<Schedule>(`${this.planningUrl}/getHeuristicSchedule`, { params });
  }

}
