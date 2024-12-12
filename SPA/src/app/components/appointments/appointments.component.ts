import { Component } from '@angular/core';
import { ListAppointmentsComponent } from '../list-appointments/list-appointments.component';
import { CreateAppointmentComponent } from '../create-appointment/create-appointment.component';

@Component({
  selector: 'app-staffs',
  standalone: true,
  imports: [ListAppointmentsComponent, CreateAppointmentComponent],
  templateUrl: './appointments.component.html',
  styleUrl: './appointments.component.css'
})
export class AppointmentsComponent {
  appointments: string[] = [];

  onAppointmentAdded(newAppointment: string) {
    this.appointments.push(newAppointment); // Adiciona novo paciente à lista
  }
}
