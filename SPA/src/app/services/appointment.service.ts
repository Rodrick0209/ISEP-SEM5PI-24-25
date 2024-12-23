import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core'
import { map, Observable, tap } from 'rxjs';
import { AuthService } from './auth.service';
import { Appointment, AppointmentEdit, AppointmentsView, AppointmentTable, MedicalTeamShowForAppointmentCreate } from '../models/appointment';
import { OperationRequest } from './operationRequestService';
import { OperationRoom } from '../models/operationRoom';
import { Staff } from '../models/staff';
import { mapToMedicalTeamShowForAppointmentCreate } from '../mappers/showMedicalTeamForCreateAppointmentMapper';


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
  private getByMedicalRecordNumber = '/api/Appointment/medicalRecordNumber'; // Update with your API URL
  private createWithMedicalTeamUrl = '/api/Appointment/CreateWithMedicalTeam'; // Update with your create API URL
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


  createAppointment(appointmentTimeSlotDtoDate: string, appointmentTimeSlotDtoTimeSlotStartMinute: string, operationRoomId: string, operationRequestId: string): Observable<any> {
    const body = {
      appointmentTimeSlotDtoDate: appointmentTimeSlotDtoDate,
      appointmentTimeSlotDtoTimeSlotStartMinute: appointmentTimeSlotDtoTimeSlotStartMinute,
      operationRoomId: operationRoomId,
      operationRequestId: operationRequestId
    }

    return this.http.post(this.createUrl, body);
  }



  createAppointmentWithMedicalTeam(
    appointmentTimeSlotDtoDate: string,
    appointmentTimeSlotDtoTimeSlotStartMinute: string,
    operationRoomId: string,
    operationRequestId: string,
    staffAnesthesyPhase: string[],
    staffSurgeryPhase: string[]
  ): Observable<any> {
    const body = {
      appointmentTimeSlotDtoDate: appointmentTimeSlotDtoDate,
      appointmentTimeSlotDtoTimeSlotStartMinute: appointmentTimeSlotDtoTimeSlotStartMinute,
      operationRoomId: operationRoomId,
      operationRequestId: operationRequestId,
      staffAnesthesyPhase: staffAnesthesyPhase,
      staffSurgeryPhase: staffSurgeryPhase
    }

    return this.http.post(this.createWithMedicalTeamUrl, body);
  }


  getTeamForAppointmentCreate(
    operationRequestId: string,
    appointmentTimeSlotStartMinute: string,
    appointmentTimeSlotDate: string
  ): Observable<MedicalTeamShowForAppointmentCreate> {
    // Usando HttpParams para enviar parâmetros como query string
    const params = new HttpParams()
      .set('operationRequestId', operationRequestId)
      .set('startMinute', appointmentTimeSlotStartMinute)
      .set('date', appointmentTimeSlotDate);

    // Fazendo a requisição GET para o endpoint
    return this.http
      .get<any>('/api/Appointment/GetStaffAvailableForDoinSurgeryAtCertainTime', { params })
      .pipe(map(data => mapToMedicalTeamShowForAppointmentCreate(data)));
  }



  editAppointment(
    id: string,
    operationRoomId: string,
    appointmentTimeSlotDtoDate: string,
    startTime: string,
    endTime: string,
    anesthesiaStaff?: string[], // Novo parâmetro para a equipe de anestesia
    surgeryStaff?: string[]   // Novo parâmetro para a equipe de cirurgia
  ): Observable<any> {
    const body: any = {
      id: id,
      operationRoomId: operationRoomId,
      appointmentTimeSlot: {
        date: appointmentTimeSlotDtoDate,
        timeSlot: {
          startTime: parseInt(startTime, 10),
          endTime: parseInt(endTime, 10)
        }
      },
      surgeryStaff: surgeryStaff || [],
      anesthesiaStaff: anesthesiaStaff || []
    };

    // Return the PUT request with the properly structured payload
    return this.http.put(`${this.url}/${id}`, body);
  }

  getAppointmentByMedicalRecordNumber(medicalRecordNumber: string): Observable<AppointmentTable[]> {
    return this.http.get<AppointmentTable[]>(`${this.getByMedicalRecordNumber}/${medicalRecordNumber}`);
  }

}
