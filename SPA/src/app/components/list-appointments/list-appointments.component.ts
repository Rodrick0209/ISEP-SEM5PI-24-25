import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import { MessageService } from '../../services/message.service';
import { AppointmentService } from '../../services/appointment.service';
import { AppointmentsView } from '../../models/appointment';  // Ajuste conforme a sua estrutura de modelo

@Component({
  selector: 'app-list-appointments',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './list-appointments.component.html',
  styleUrls: ['./list-appointments.component.css']
})
export class ListAppointmentsComponent implements OnInit {
  successMessage: string | null = null;
  errorMessage: string | null = null;

  appointments: AppointmentsView[] = [];
  paginatedAppointments: AppointmentsView[] = [];

  currentPage: number = 1;
  itemsPerPage: number = 10;
  totalPages: number = 0;

  constructor(
    private appointmentService: AppointmentService,
    private router: Router,
    private messageService: MessageService
  ) {}

  ngOnInit(): void {
    this.loadAppointments();
  }

  loadAppointments(): void {
    this.appointmentService.getAppointments().subscribe({
      next: (data: AppointmentsView[]) => {
        this.appointments = data;
        this.totalPages = Math.ceil(this.appointments.length / this.itemsPerPage);
        this.updatePagination();
      },
      error: (err: any) => {
        console.error('Failed to load appointments', err);
        this.errorMessage = 'Failed to load appointments';
      }
    });
  }

  updatePagination(): void {
    const startIndex = (this.currentPage - 1) * this.itemsPerPage;
    this.paginatedAppointments = this.appointments.slice(startIndex, startIndex + this.itemsPerPage);
  }

  nextPage(): void {
    if (this.currentPage < this.totalPages) {
      this.currentPage++;
      this.updatePagination();
    }
  }

  previousPage(): void {
    if (this.currentPage > 1) {
      this.currentPage--;
      this.updatePagination();
    }
  }

  createAppointment(): void {
    this.router.navigate(['/appointments/create']);
  }

  editAppointment(appointment: AppointmentsView): void {
    this.router.navigate(['/appointments/edit', appointment.id]);
  }

}
