import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { AppointmentService } from '../../services/appointment.service';
import { MessageService } from '../../services/message.service';
import { MedicalTeamShowForAppointmentCreate } from '../../models/appointment';
import { OperationRoom } from '../../models/operationRoom';
import { OperationRequest } from '../../services/operationRequestService';
import { SpecializationService } from '../../services/specialization.service';
import { Specialization } from '../../models/specialization';

@Component({
  selector: 'app-create-appointment',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './create-appointment.component.html',
  styleUrls: ['./create-appointment.component.css']
})
export class CreateAppointmentComponent {
  
  // Enums e listas para seleção
medicalTeamToShows: MedicalTeamShowForAppointmentCreate = {
  staffAnesthesyPhase: [],
  staffSurgeryPhase: []
};

  loadingMedicalTeam: boolean = false; // Flag de carregamento
  operationRooms: OperationRoom[] = [];
  operationRequests: OperationRequest[] = [];
  specializationNames: { [key: string]: string } = {};

  staffAnestesiaEscolhido: { [specializationId: string]: string[] } = {}; 
  staffCirurgiaEscolhido: { [specializationId: string]: string[] } = {}; 
  IsMedicalTeamShowForAppointmentCreate: boolean = false;  
  // Dados do formulário
  submitForm = {
    appointmentTimeSlotDate: '',
    appointmentTimeSlotStartMinute: '',
    operationRoomId: '',
    operationRequestId: '',
  };

  showConfirmation: boolean = false;
  errorMessage: string = '';

  constructor(
    private router: Router,
    private appointmentService: AppointmentService,
    private messageService: MessageService,
    private specializationsService: SpecializationService
  ) { }

  ngOnInit(): void {
    this.loadOperationRooms();
    this.loadOperationRequests();
  }

  loadOperationRooms(): void {
    this.appointmentService.getOperationRooms().subscribe(
      (rooms: OperationRoom[]) => {
        this.operationRooms = rooms;
        console.log('Number of operation rooms:', this.operationRooms.length); // Exibe a quantidade de operation rooms no console
      },
      (error) => {
        console.error('Erro ao obter as salas de operação:', error);
      }
    );
  }

  loadOperationRequests(): void {
    this.appointmentService.getOperationRequestsAvailable().subscribe({
      next: requests => this.operationRequests = requests,
      error: err => console.error('Failed to load operation requests', err)
    });
  }

  loadMedicalTeamToShow(): void {
    const { appointmentTimeSlotStartMinute, operationRequestId, appointmentTimeSlotDate } = this.submitForm;
    if (appointmentTimeSlotStartMinute && operationRequestId && appointmentTimeSlotDate) {

      this.loadingMedicalTeam = true;
      this.IsMedicalTeamShowForAppointmentCreate = true;
      this.appointmentService.getTeamForAppointmentCreate(operationRequestId, appointmentTimeSlotStartMinute, appointmentTimeSlotDate).subscribe({
        next: medicalTeamToShow => {
          this.errorMessage = '';
          this.medicalTeamToShows = medicalTeamToShow;
          this.loadSpecializationNames();
          this.loadingMedicalTeam = false;
        },
        error: err => {
          console.error('Failed to load medical team to show', err);
          this.errorMessage = 'Please choose another date or start time, there is no medical team available for the selected date and time.';
          this.loadingMedicalTeam = false;
        }
      });
    } else {
      console.warn('Required fields are missing to load the medical team.');
      this.loadingMedicalTeam = false;
    }
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

      const staffAnesthesyPhase = Object.values(this.staffAnestesiaEscolhido).flat();
      console.log('staffAnestesiaIds', staffAnesthesyPhase);
      const staffSurgeryPhase = Object.values(this.staffCirurgiaEscolhido).flat();
      console.log('staffCirurgiaIds', staffSurgeryPhase);

      this.appointmentService.createAppointmentWithMedicalTeam(
        appointmentData.appointmentTimeSlotDate,
        appointmentData.appointmentTimeSlotStartMinute,
        appointmentData.operationRoomId,
        appointmentData.operationRequestId,
        staffAnesthesyPhase,
        staffSurgeryPhase
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


  isSelected(staff: string, phase: string, specializationId: string): boolean {
    if (phase === 'anesthesia') {
      return this.staffAnestesiaEscolhido[specializationId]?.includes(staff) || false;
    }
    if (phase === 'surgery') {
      return this.staffCirurgiaEscolhido[specializationId]?.includes(staff) || false;
    }
    return false;
  }

  // Função para verificar se atingiu o número máximo de seleções para a especialidade
  isMaxSelected(phase: string, specializationId: string, nrNeeded: number): boolean {
    if (phase === 'anesthesia') {
      return this.staffAnestesiaEscolhido[specializationId]?.length >= nrNeeded;
    }
    if (phase === 'surgery') {
      return this.staffCirurgiaEscolhido[specializationId]?.length >= nrNeeded;
    }
    return false;
  }

  // Função para alternar a seleção de um staff
  toggleStaffSelection(staff: string, phase: string, specializationId: string, nrNeeded: number): void {
    if (phase === 'anesthesia') {
      const isAlreadySelected = this.staffAnestesiaEscolhido[specializationId]?.includes(staff);

      if (isAlreadySelected) {
        // Se o staff já estiver selecionado, desmarque-o
        this.staffAnestesiaEscolhido[specializationId] = this.staffAnestesiaEscolhido[specializationId].filter(item => item !== staff);
      } else {
        // Se o staff não estiver selecionado e o número de seleções não ultrapassou o limite
        if (!this.isMaxSelected('anesthesia', specializationId, nrNeeded)) {
          this.staffAnestesiaEscolhido[specializationId] = [...(this.staffAnestesiaEscolhido[specializationId] || []), staff];
        }
      }
    }

    if (phase === 'surgery') {
      const isAlreadySelected = this.staffCirurgiaEscolhido[specializationId]?.includes(staff);

      if (isAlreadySelected) {
        // Se o staff já estiver selecionado, desmarque-o
        this.staffCirurgiaEscolhido[specializationId] = this.staffCirurgiaEscolhido[specializationId].filter(item => item !== staff);
      } else {
        // Se o staff não estiver selecionado e o número de seleções não ultrapassou o limite
        if (!this.isMaxSelected('surgery', specializationId, nrNeeded)) {
          this.staffCirurgiaEscolhido[specializationId] = [...(this.staffCirurgiaEscolhido[specializationId] || []), staff];
        }
      }
    }
  }


  loadSpecializationNames(): void {
    const specializationIds = new Set<string>();
    this.medicalTeamToShows.staffAnesthesyPhase.forEach(phase => specializationIds.add(phase.specializationId));
    this.medicalTeamToShows.staffSurgeryPhase.forEach(phase => specializationIds.add(phase.specializationId));

    specializationIds.forEach(id => {
      this.specializationsService.getSpecializationById(id).subscribe({
        next: (specialization: Specialization) => {
          this.specializationNames[id] = specialization.name;
        },
        error: (err: any) => {
          console.error('Failed to load specialization', err);
        }
      });
    });
  }

  getSpecializationName(specializationId: string): string {
    return this.specializationNames[specializationId] || 'Unknown';
  }


}