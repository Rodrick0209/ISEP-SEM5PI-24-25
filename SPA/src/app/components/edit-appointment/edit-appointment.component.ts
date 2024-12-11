import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { AppointmentService } from '../../services/appointment.service';
import { StaffService } from '../../services/staff.service';
import { MessageService } from '../../services/message.service';

@Component({
  selector: 'app-edit-appointment',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './edit-appointment.component.html',
  styleUrls: ['./edit-appointment.component.css']
})
export class EditAppointmentComponent implements OnInit {
  submitForm = {
    appointmentTimeSlotDtoDate: '',
    appointmentTimeSlotDtoTimeSlotStartMinute: '',
    appointmentTimeSlotDtoTimeSlotEndMinute: '',
    operationRoomId: '',
    operationRequestId: '',
    appointmentStatus: '',
    operationRequestTeamForAnesthesy: [] as string[],
    operationRequestTeamForSurgery: [] as string[]
  };

  id: string | null = null;
  errorMessage: string = '';
  showConfirmation: boolean = false;
  availableStaff: any[] = []; // Lista de equipes obtida do `getStaff`

  constructor(
    private router: Router,
    private appointmentService: AppointmentService,
    private route: ActivatedRoute,
    private messageService: MessageService
  ) {}

  ngOnInit(): void {
    this.id = this.route.snapshot.paramMap.get('id');

    if (this.id) {
      // Obtém os detalhes do agendamento a editar
      this.appointmentService.getAppointmentByIdEdit(this.id).subscribe(
        (appointment) => {
          this.submitForm = {
            appointmentTimeSlotDtoDate: appointment.appointmentTimeSlot.date,
            appointmentTimeSlotDtoTimeSlotStartMinute: appointment.appointmentTimeSlot.startTime,
            appointmentTimeSlotDtoTimeSlotEndMinute: appointment.appointmentTimeSlot.endTime,
            operationRoomId: appointment.operationRoomId,
            operationRequestId: appointment.operationRequestId,
            appointmentStatus: appointment.appointmentStatus,
            operationRequestTeamForAnesthesy: appointment.operationRequestTeamForAnesthesy || [],
            operationRequestTeamForSurgery: appointment.operationRequestTeamForSurgery || []
          };
        },
        (error) => {
          this.errorMessage = 'Failed to fetch appointment details.';
          console.error(error);
        }
      );
    }

    // Carrega a lista de equipes
    this.appointmentService.getStaff().subscribe(
      (staffList) => {
        this.availableStaff = staffList;
      },
      (error) => {
        console.error('Failed to fetch staff list', error);
      }
    );
  }

  confirmSubmission(): void {
    this.showConfirmation = true;
  }

  closeConfirmationModal(): void {
    this.showConfirmation = false;
  }

  onSubmit(appointmentForm: any): void {
    this.showConfirmation = false;
    if (appointmentForm.valid) {
      const appointmentData = {
        ...this.submitForm
      };

      if (this.id) {
        this.appointmentService
          .editAppointment(
            this.id,
            appointmentData.operationRequestId,
            appointmentData.operationRoomId,
            appointmentData.appointmentTimeSlotDtoDate,
            appointmentData.appointmentTimeSlotDtoTimeSlotStartMinute,
            appointmentData.appointmentTimeSlotDtoTimeSlotEndMinute,
            appointmentData.appointmentStatus,
            appointmentData.operationRequestTeamForAnesthesy,
            appointmentData.operationRequestTeamForSurgery
          )
          .subscribe(
            (response) => {
              console.log('Appointment edited successfully', response);
              this.messageService.setMessage(`Appointment ${this.id} successfully edited!`);
              this.router.navigate(['/appointments']);
            },
            (error) => {
              this.errorMessage = error.error.message;
              console.error('Failed to edit appointment', error);
            }
          );
      } else {
        this.errorMessage = 'Id is missing.';
      }
    }
  }

  onCancel(): void {
    this.showConfirmation = false;
    this.router.navigate(['/appointments']);
  }
}
