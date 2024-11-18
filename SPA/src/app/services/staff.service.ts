import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs';
import { AuthService } from './auth.service';
import { Staff, StaffsView } from '../models/staff';


@Injectable({
  providedIn: 'root'
})
export class StaffService {
    private getAll = '/api/Staff/GetAllForUi'; // Update with your API URL
    private filterApiUrl = '/api/Staff/search'; // Update with your filter API URL
    private url = '/api/Staff'
    private createUrl = '/api/Staff/CreateUi'
  constructor(private http: HttpClient) { }

  getStaffs(): Observable<StaffsView[]> {
    return this.http.get<StaffsView[]>(this.getAll);
  }

  getStaffById(id: string): Observable<Staff> {
    return this.http.get<Staff>(`${this.url}/${id}`);
  }


  createStaff(fullName : string, licenseNumber : string, specialization : string, email : string, phoneNumber : string, category : string): Observable<any> {
    const body = {
        fullName: fullName,
        licenseNumber: licenseNumber,
        specialization: specialization,
        email: email,
        phoneNumber: phoneNumber,
        category: category
    }

    return this.http.post(this.createUrl, body);
  }

  editStaff(id: string, fullName: string, licenseNumber : string, phoneNumber : string, email: string) : Observable<any> {
    const body: any = {};
    body.id = id;
    if (fullName) body.fullName = fullName;
    if (licenseNumber) body.licenseNumber = licenseNumber;
    if (phoneNumber) body.phoneNumber = phoneNumber;
    if (email) body.email = email;
    
    return this.http.patch(`${this.url}/${id}`, body);
  }

  deleteStaff(id: string) : Observable<any> {
    return this.http.delete(`${this.url}/${id}`);
  }

  filterStaffs(name: string, licenseNumber: string, phoneNumber: string, email: string, specialization : string): Observable<StaffsView[]> {
    let params = new HttpParams();
    if (name) {
      params = params.set('name', name);
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
    if (specialization) {
        params = params.set('specialization', specialization);
      }

    return this.http.get<StaffsView[]>(this.filterApiUrl, { params });
  }
}
