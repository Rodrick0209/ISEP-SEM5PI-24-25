import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable,tap } from 'rxjs';



export interface SurgeryRoom {
  roomNumber: string;
}




@Injectable({
  providedIn: 'root'
})
export class PlanningService {
  private greetUrl = "http://localhost:8080/greet"



  


  constructor(private http: HttpClient) { }

  greet(name: string) {
    let params = new HttpParams()

    if (name) {
      params = params.append('name', name)
    }

    return this.http.get(`${this.greetUrl}`, { params });
  }


  getSurgeryRooms(): Observable<SurgeryRoom[]> {
    return this.http.get<SurgeryRoom[]>("/api/OperationRoom/GetAll");
  }




}