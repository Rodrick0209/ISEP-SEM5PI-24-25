import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core'
import { map, Observable, tap } from 'rxjs';
import { AuthService } from './auth.service';
import { Specialization } from '../models/specialization';
import { SpecializationMapper } from '../mappers/specialization.mapper';


@Injectable({
  providedIn: 'root'
})

export class SpecializationService {
    private baseUrl = '/api/Specializations'; // Update with your create API URL

    private getSpecializationUrl = '/GetAll' 
    private getSpecializationFilteredUrl = '/GetFiltered'

    constructor(private http: HttpClient) { }


    createSpecialization(name: string): Observable<Specialization> {
      const headers = new HttpHeaders({ 'Content-Type': 'application/json' });
      const body = JSON.stringify(SpecializationMapper.mapToBackendFormat(name));

      console.log('Request body being sent:', body);      // Log para verificar o formato do JSON sendo enviado
          
      return this.http.post<any>(this.baseUrl, body, { headers }).pipe(
        tap(data => {
          console.log('Raw response from POST:', data); // Debug: log the raw response
        }),
        map(data => SpecializationMapper.mapToSpecialization(data))
      );
    }

    getSpecializations(): Observable<Specialization[]> {
        const url = `${this.baseUrl}${this.getSpecializationUrl}`;
        return this.http.get<any[]>(url).pipe(
        map(data => SpecializationMapper.mapToSpecializations(data))
        );
    }


    getSpecializationsFiltered(name: string): Observable<Specialization[]> {
      const params = new HttpParams().set('Name', name); // Envia 'Name' como parâmetro de consulta
  
      const url = `${this.baseUrl}${this.getSpecializationFilteredUrl}`;
      return this.http.get<any[]>(url, { params }).pipe(
        map(data => SpecializationMapper.mapToSpecializations(data))
      );
    }

    editSpecialization(id: string, name: string): Observable<Specialization> {
      return this.http.put<any>(`${this.baseUrl}/${id}`, SpecializationMapper.mapToEditSpecialization(id,name)).pipe(
        map(data => SpecializationMapper.mapToSpecialization(data))
      );
    }


    deleteSpecialization(id: string): Observable<Specialization> {
      return this.http.delete<any>(`${this.baseUrl}/${id}`);
    }




}