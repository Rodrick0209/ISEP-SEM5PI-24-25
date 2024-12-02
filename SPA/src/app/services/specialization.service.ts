import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core'
import { Observable, tap } from 'rxjs';
import { AuthService } from './auth.service';
import { Specialization } from '../models/specialization';
import { SpecializationMapper } from '../mappers/specialization.mapper';


@Injectable({
  providedIn: 'root'
})

export class SpecializationService {
    private baseUrl = '/api/Specializations'; // Update with your create API URL

    constructor(private http: HttpClient) { }


    createSpecialization(name: string): Observable<Specialization> {
        const body = {
          Name: name
        };
    
        return this.http.post<Specialization>(this.baseUrl, body);
      }

}