import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core'
import { Observable, tap } from 'rxjs';
import { AuthService } from './auth.service';
import { Appointment, AppointmentEdit, AppointmentsView } from '../models/appointment';
import { OperationRequest } from './operationRequestService';
import { OperationRoom } from '../models/operationRoom';
import { Staff } from '../models/staff';


@Injectable({
  providedIn: 'root'
})
export class AppointmentService {
  private getAllAppointments = '/api/Appointment/GetAllUI'; // Update with your API URL
  private url = '/api/Appointment';
  private createUrl = '/api/Appointment'; // Update with your create API URL
  private getAppByIdUrl = '/api/Appointment'; // Update with your get by ID API URL
  private getAllOpRquestsAvailable = '/api/OperationRequest/GetAllAvailableForUi';
  private getAllOpRooms = '/api/OperationRoom/GetAll';
  private getAllStaff = '/api/Staff/GetAllForUi';
  constructor(private http: HttpClient) { }



  getAppointments(): Observable<AppointmentsView[]> {
    return this.http.get<AppointmentsView[]>(this.getAllAppointments);
  }

  getAppointmentById(id: string): Observable<Appointment> {
    return this.http.get<Appointment>(`${this.getAppByIdUrl}/${id}`);
  }

  getAppointmentByIdEdit(id: string): Observable<AppointmentEdit> {
    return this.http.get<AppointmentEdit>(`${this.getAppByIdUrl}/${id}`);
  }

  getOperationRequestsAvailable(): Observable<OperationRequest[]> {
    return this.http.get<OperationRequest[]>(this.getAllOpRquestsAvailable);
  }

  getOperationRooms(): Observable<OperationRoom[]> {
    return this.http.get<OperationRoom[]>(this.getAllOpRooms);
  }

  getStaff(): Observable<Staff[]> {
    return this.http.get<Staff[]>(this.getAllStaff);
  }


  createAppointment(appointmentTimeSlotDtoDate: string, appointmentTimeSlotDtoTimeSlotStartMinute: string, appointmentTimeSlotDtoTimeSlotEndMinute: string, operationRoomId: string, operationRequestId: string): Observable<any> {
    const body = {
      appointmentTimeSlotDtoDate: appointmentTimeSlotDtoDate,
      appointmentTimeSlotDtoTimeSlotStartMinute: appointmentTimeSlotDtoTimeSlotStartMinute,
      appointmentTimeSlotDtoTimeSlotEndMinute: appointmentTimeSlotDtoTimeSlotEndMinute,
      operationRoomId: operationRoomId,
      operationRequestId: operationRequestId
    }

    return this.http.post(this.createUrl, body);
  }

  editAppointment(
    id: string,
    operationRequestId: string,
    operationRoomId: string,
    appointmentTimeSlotDtoDate: string,
    appointmentTimeSlotDtoTimeSlotStartMinute: string,
    appointmentTimeSlotDtoTimeSlotEndMinute: string,
    appointmentStatus: string,
    operationRequestTeamForAnesthesy?: string[], // Novo parâmetro para a equipe de anestesia
    operationRequestTeamForSurgery?: string[]   // Novo parâmetro para a equipe de cirurgia
  ): Observable<any> {
    const body: any = {};
    body.id = id;

    if (operationRequestId) body.operationRequestId = operationRequestId;
    if (operationRoomId) body.operationRoomId = operationRoomId;
    if (appointmentTimeSlotDtoDate) body.appointmentTimeSlotDtoDate = appointmentTimeSlotDtoDate;
    if (appointmentTimeSlotDtoTimeSlotStartMinute) body.appointmentTimeSlotDtoTimeSlotStartMinute = appointmentTimeSlotDtoTimeSlotStartMinute;
    if (appointmentTimeSlotDtoTimeSlotEndMinute) body.appointmentTimeSlotDtoTimeSlotEndMinute = appointmentTimeSlotDtoTimeSlotEndMinute;
    if (appointmentStatus) body.appointmentStatus = appointmentStatus;

    // Adicionando as novas listas, se existirem
    if (operationRequestTeamForAnesthesy && operationRequestTeamForAnesthesy.length > 0) {
      body.operationRequestTeamForAnesthesy = operationRequestTeamForAnesthesy;
    }
    if (operationRequestTeamForSurgery && operationRequestTeamForSurgery.length > 0) {
      body.operationRequestTeamForSurgery = operationRequestTeamForSurgery;
    }

    return this.http.put(`${this.url}/${id}`, body);
  }



}
