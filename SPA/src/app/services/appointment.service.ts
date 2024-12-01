/*import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core'
import { Observable, tap } from 'rxjs';
import { AuthService } from './auth.service';
import { Appointment} from '../models/appointment';


@Injectable({
  providedIn: 'root'
})
export class AppointmentService {
    private getAll = '/api/Appointment/GetAll'; // Update with your API URL
    private url = '/api/Appointment'
    private createUrl = '/api/Appointment'; // Update with your create API URL
    private getStaffByIdUrl = 'api/Staff/GetByIdForUI'; // Update with your get by ID API URL
  constructor(private http: HttpClient) { }

  

  getStaffs(): Observable<StaffsView[]> {
    return this.http.get<StaffsView[]>(this.getAll).pipe(
      tap(data => console.log('Dados brutos recebidos da API:', data))
    );
  }

  getStaffById(id: string): Observable<Staff> {
    return this.http.get<Staff>(`${this.getStaffByIdUrl}/${id}`);
  }


  createStaff(fullName : string, licenseNumber : string, specializationId : string, email : string, phoneNumber : string, category : string): Observable<any> {
    const body = {
        fullName: fullName,
        licenseNumber: licenseNumber,
        specializationId: specializationId,
        email: email,
        phoneNumber: phoneNumber,
        category: category
    }

    return this.http.post(this.createUrl, body);
  }

  editStaff(id: string, fullName: string, licenseNumber : string, phoneNumber : string, email: string, specializationId : string) : Observable<any> {
    const body: any = {};
    body.id = id;
    if (fullName) body.fullName = fullName;
    if (licenseNumber) body.licenseNumber = licenseNumber;
    if (phoneNumber) body.phoneNumber = phoneNumber;
    if (email) body.email = email;
    if (specializationId) body.specializationId = specializationId;
    
    return this.http.put(`${this.url}/${id}`, body);
  }

  deleteStaff(id: string) : Observable<any> {
    return this.http.delete(`${this.url}/${id}`);
  }

  filterStaffs(fullName: string, licenseNumber: string, phoneNumber: string, email: string, specializationId : string): Observable<StaffsView[]> {
    let params = new HttpParams();
    if (fullName) {
      params = params.set('name', fullName);
    }
    if (licenseNumber) {
      params = params.set('licenseNumber', licenseNumber);
    }
    if (phoneNumber) {
      params = params.set('phoneNumber', phoneNumber);
    }
    if (email) {
      params = params.set('email', email);
    }
    if (specializationId) {
        params = params.set('specialization', specializationId);
      }

    return this.http.get<StaffsView[]>(this.filterApiUrl, { params });
  }
}*/
