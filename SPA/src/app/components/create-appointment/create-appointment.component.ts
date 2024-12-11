import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { AppointmentService } from '../../services/appointment.service';
import { MessageService } from '../../services/message.service';
import { AppointmentStatus } from '../../models/appointment';
import { OperationRoom } from '../../models/operationRoom';
import { OperationRequest } from '../../services/operationRequestService';

@Component({
  selector: 'app-create-appointment',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './create-appointment.component.html',
  styleUrls: ['./create-appointment.component.css']
})
export class CreateAppointmentComponent {
  
  // Enums e listas para seleção
  AppointmentStatus = AppointmentStatus;
  statusOptions: string[] = Object.values(AppointmentStatus);

  operationRooms: OperationRoom[] = [];
  operationRequests: OperationRequest[] = [];
  
  // Dados do formulário
  submitForm = {
    appointmentTimeSlotDate: '',
    appointmentTimeSlotStartMinute: '',
    appointmentTimeSlotEndMinute: '',
    operationRoomId: '',
    operationRequestId: '',
    appointmentStatus: AppointmentStatus.Scheduled
  };

  showConfirmation: boolean = false;
  errorMessage: string = '';

  constructor(
    private router: Router,
    private appointmentService: AppointmentService,
    private messageService: MessageService
  ) { }

  ngOnInit(): void {
    this.loadOperationRooms();
    this.loadOperationRequests();
  }

  loadOperationRooms(): void {
    this.appointmentService.getOperationRooms().subscribe({
      next: rooms => this.operationRooms = rooms,
      error: err => console.error('Failed to load operation rooms', err)
    });
  }

  loadOperationRequests(): void {
    this.appointmentService.getOperationRequestsAvailable().subscribe({
      next: requests => this.operationRequests = requests,
      error: err => console.error('Failed to load operation requests', err)
    });
  }

  confirmSubmission(): void {
    this.showConfirmation = true; // Mostrar modal de confirmação
  }

  closeConfirmationModal(): void {
    this.showConfirmation = false; // Fechar modal de confirmação
  }

  onSubmit(appointmentForm: any): void {
    this.showConfirmation = false;

    if (appointmentForm.valid) {
      const appointmentData = {
        ...this.submitForm
      };

      this.appointmentService.createAppointment(
        appointmentData.appointmentTimeSlotDate,
        appointmentData.appointmentTimeSlotStartMinute,
        appointmentData.appointmentTimeSlotEndMinute,
        appointmentData.operationRoomId,
        appointmentData.operationRequestId
      ).subscribe({
        next: (data: any) => {
          console.log('Appointment created successfully', data);
          this.messageService.setMessage('Appointment successfully created!');
          this.router.navigate(['/appointments']);
        },
        error: (err: any) => {
          console.error('Failed to create appointment', err);
          this.errorMessage = 'Failed to create appointment: ' + (err.error?.message || 'Unknown error');
        }
      });
    } else {
      console.error('Form is invalid');
    }
  }

  onCancel(): void {
    this.showConfirmation = false;
    this.router.navigate(['/appointments']);
  }
}
