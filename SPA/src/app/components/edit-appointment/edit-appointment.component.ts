import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { AppointmentService } from '../../services/appointment.service';
import { StaffService } from '../../services/staff.service';
import { MessageService } from '../../services/message.service';
import { OperationRoom } from '../../models/operationRoom';
import { OperationRequest } from '../../services/operationRequestService';

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
    startTime: '',
    endTime: '',
    operationRoomId: '',
    operationRequestId: '',
    AppointmentStatus: '',
    operationRequestTeamForAnesthesy: [] as string[],
    operationRequestTeamForSurgery: [] as string[],
    operationRequestTeamForCleaning: [] as string[]
  };

  id: string | null = null;
  errorMessage: string = '';
  showConfirmation: boolean = false;
  availableStaff: any[] = []; // Lista de equipes obtida do `getStaff`
  operationRooms: OperationRoom[] = [];
  operationRequests: OperationRequest[] = []; // Lista de salas de operação 

  constructor(
    private router: Router,
    private appointmentService: AppointmentService,
    private route: ActivatedRoute,
    private messageService: MessageService
  ) { }

  ngOnInit(): void {
    this.id = this.route.snapshot.paramMap.get('id');

    if (this.id) {
      // Obtém os detalhes do agendamento a editar
      this.appointmentService.getAppointmentByIdEdit(this.id).subscribe(
        (appointment) => {
          
          this.submitForm = {
            appointmentTimeSlotDtoDate: appointment.appointmentTimeSlot.date,
            startTime: appointment.appointmentTimeSlot.timeSlot.startTime,
            endTime: appointment.appointmentTimeSlot.timeSlot.endTime,
            operationRoomId: appointment.operationRoomId,
            operationRequestId: appointment.id,
            AppointmentStatus: appointment.AppointmentStatus,
            operationRequestTeamForAnesthesy: appointment.anesthesiaStaff || [],
            operationRequestTeamForSurgery: appointment.surgeryStaff || [],
            operationRequestTeamForCleaning: appointment.operationRequestTeamForCleaning || []
          };
          
        },
        (error) => {
          this.errorMessage = 'Failed to fetch appointment details.';
          console.error(error);
        }
        
      );
      

    }

    this.appointmentService.getOperationRooms().subscribe(
      (rooms) => {
        this.operationRooms = rooms;
      },
      (error) => {
        console.error('Failed to fetch operation rooms', error);
      }
    );

    // Carrega a lista de equipes
    this.appointmentService.getStaff().subscribe(
      (staffList) => {
        console.log('Staff List:', staffList);
        this.availableStaff = staffList;
      },
      (error) => {
        console.error('Failed to fetch staff list', error);
      }
    );

    // **Carrega a lista de Operation Requests**
    this.appointmentService.getOperationRequestsAvailable().subscribe(
      (requestsList) => {
        console.log('Operation Requests:', requestsList);
        this.operationRequests = requestsList; // Certifique-se de que esta variável exista no componente
      },
      (error) => {
        console.error('Failed to fetch operation requests', error);
        this.errorMessage = 'Failed to load operation requests.';
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
            appointmentData.operationRoomId,
            appointmentData.appointmentTimeSlotDtoDate,
            appointmentData.startTime,
            appointmentData.endTime,
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

  get appointmentTimeSlotStart(): string {
    // Convert the stored minute value to a time format (HH:mm)
    return this.convertMinutesToTime(this.submitForm.startTime);
  }

  get appointmentTimeSlotEnd(): string {
    // Convert the stored minute value to a time format (HH:mm)
    return this.convertMinutesToTime(this.submitForm.endTime);
  }

  set appointmentTimeSlotStart(value: string) {
    // Convert the time format (HH:mm) back to minutes
    this.submitForm.startTime = this.convertTimeToMinutes(value);
  }

  set appointmentTimeSlotEnd(value: string) {
    // Convert the time format (HH:mm) back to minutes
    this.submitForm.endTime = this.convertTimeToMinutes(value);
  }

  convertMinutesToTime(totalMinutes: string): string {
    const time = parseInt(totalMinutes, 10);
    const hours = Math.floor(time / 60);
    const minutes = time % 60;
    return `${hours.toString().padStart(2, '0')}:${minutes.toString().padStart(2, '0')}`;
  }

  convertTimeToMinutes(time: string): string {
    const [hours, minutes] = time.split(':').map(Number);
    return (hours * 60 + minutes).toString();
  }

  selectedAnesthetist: string = '';

  // Add the selected anesthetist to the team
  addAnesthetist(): void {
    if (
      this.selectedAnesthetist &&
      !this.submitForm.operationRequestTeamForAnesthesy.includes(
        this.selectedAnesthetist
      )
    ) {
      this.submitForm.operationRequestTeamForAnesthesy.push(
        this.selectedAnesthetist
      );
      // Optionally remove them from the available list
      this.availableStaff = this.availableStaff.filter(
        (staff) => staff.d !== this.selectedAnesthetist
      );
      this.selectedAnesthetist = ''; // Reset selection
    }
  }

  // Remove an anesthetist from the team
  removeAnesthetist(staff: string): void {
    this.submitForm.operationRequestTeamForAnesthesy = this.submitForm.operationRequestTeamForAnesthesy.filter(
      (member) => member !== staff
    );
    // Optionally add them back to the available list
    if (!this.availableStaff.includes(staff)) {
      this.availableStaff.push(staff);
    }
  }


  selectedSurgeon: string = '';

  // Add the selected surgeon to the team
  addSurgeon(): void {
    if (
      this.selectedSurgeon &&
      !this.submitForm.operationRequestTeamForSurgery.includes(
        this.selectedSurgeon
      )
    ) {
      this.submitForm.operationRequestTeamForSurgery.push(
        this.selectedSurgeon
      );
      // Optionally remove them from the available list
      this.availableStaff = this.availableStaff.filter(
        (staff) => staff.d !== this.selectedSurgeon
      );
      this.selectedSurgeon = ''; // Reset selection
    }
  }

  // Remove a surgeon from the team
  removeSurgeon(staff: string): void {
    this.submitForm.operationRequestTeamForSurgery = this.submitForm.operationRequestTeamForSurgery.filter(
      (member) => member !== staff
    );
    // Optionally add them back to the available list
    if (!this.availableStaff.includes(staff)) {
      this.availableStaff.push(staff);
    }
  }

}
