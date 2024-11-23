import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

export interface SurgeryRoom {
  roomNumber: string;
}

// Representa uma tupla no agOpRoomBetter
export interface OperationSegment {
  start: number;
  end: number;
  operationId: string;
}

// Interface principal do Schedule
export interface Schedule {
  day: string;
  room: string;
  agOpRoomBetter: OperationSegment[]; // Lista de intervalos de operação
}


@Injectable({
  providedIn: 'root',
})
export class PlanningService {
  private planningUrl = 'http://localhost:8080/planning'; // URL base para o módulo de planeamento

  constructor(private http: HttpClient) {}

  greet(name: string) {
    let params = new HttpParams();

    if (name) {
      params = params.append('name', name);
    }

    return this.http.get(`${this.planningUrl}/greet`, { params });
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
