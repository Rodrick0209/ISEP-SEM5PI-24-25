import { Component, OnInit } from '@angular/core';
import { MarkXComponent } from '../template/mark-x/mark-x.component';
import { CommonModule } from '@angular/common';
import { PatientService } from '../../services/patient.service';
import { AppointmentService } from '../../services/appointment.service';
import { Allergy, MedicalCondition, MedicalRecord, Patient } from '../../models/patient';
import { Appointment, AppointmentsView, AppointmentTable } from '../../models/appointment';
import { ActivatedRoute, Router } from '@angular/router';
import { EditConfirmationComponent } from '../edit-confirmation/edit-confirmation.component';
import { FormsModule } from '@angular/forms';
import { AuthService } from '../../services/auth.service';

@Component({
  selector: 'app-download-medical-history',
  standalone: true,
  imports: [MarkXComponent, CommonModule, FormsModule],
  templateUrl: './download-medical-history.component.html',
  styleUrl: './download-medical-history.component.css'
})
export class DownloadMedicalHistoryComponent implements OnInit {
  password: string = ''; // Password of the patient
  email: string = ''; // Email of the patient
  errorMessage: string | null = null; // Error message to display
  patient: Patient | null = null; // Patient data
  isSelectedMedicalRecord: boolean = false; // Tracks selection of Medical Record
  isSelectedAppointmentHistory: boolean = false; // Tracks selection of Appointment History
  medicalRecord: MedicalRecord | null = null; // Medical Record data
  appointments: AppointmentTable[] = []; // Appointments data
  isDownloading: boolean = false;

  constructor(private patientService: PatientService, private appointmentService: AppointmentService, private route: ActivatedRoute, private authService: AuthService, private router: Router) { }

  ngOnInit(): void {
    this.email = this.route.snapshot.paramMap.get('email') || '';
    var emailVerified = this.authService.extractEmailFromToken();
    if (emailVerified != this.email) {
      this.router.navigate(['/home']);
    }
  }

  onMarkMedicalRecordClick(): void {
    this.isSelectedMedicalRecord = !this.isSelectedMedicalRecord; // Toggle selection state
  }

  onMarkAppointementHistoryClick(): void {
    this.isSelectedAppointmentHistory = !this.isSelectedAppointmentHistory; // Toggle selection state
  }

  async onDownload(): Promise<void> {
    if (!this.password) {
      this.errorMessage = 'Password is required to download the medical history.';
      this.isDownloading = false;
      return;
    }

    const isValidPassword = await this.authService.validatePassword(this.email, this.password);
    if (!isValidPassword) {
      this.errorMessage = 'Invalid password. Please try again.';
      this.isDownloading = false;
      return;
    }

    if (this.isDownloading) {
      return;
    }

    this.isDownloading = true;

    this.errorMessage = null;

    this.patient = await this.getPatientInformation();
    this.medicalRecord = await this.getMedicalRecord(this.patient.medicalRecordNumber);
    this.appointments = await this.getAppointmentHistoryByPatient(this.patient.medicalRecordNumber);

    // Create the JSON object
    const medicalHistory = {
      medicalHistory: {
        patientInformation: {
          medicalRecordNumber: this.patient?.medicalRecordNumber,
          name: this.patient?.name,
          dateOfBirth: this.patient?.dateOfBirth,
          gender: this.patient?.gender,
          email: this.patient?.email,
          phoneNumber: this.patient?.phoneNumber,
          address: this.patient?.address,
          emergencyContact: this.patient?.emergencyContact
        },
        medicalRecord: {
          conditions: this.medicalRecord?.medicalConditions.map(condition => ({
            name: condition.name,
            date: condition.date ? new Date(condition.date).toISOString().split('T')[0] : 'N/A'
          })),
          allergies: this.medicalRecord?.allergies.map(allergy => ({
            name: allergy.name,
            description: allergy.description || 'N/A'
          }))
        },
        appointments: this.appointments.map(appointment => ({
          priority: appointment.priority,
          date: appointment.appointmentTimeSlot.date,
          doctor: appointment.doctor,
          startTime: this.formatTime(Number(appointment.appointmentTimeSlot.timeSlot.startTime)),
          endTime: this.formatTime(Number(appointment.appointmentTimeSlot.timeSlot.endTime)),
          roomNumber: appointment.roomNumber
        }))
      }
    };

    // Convert JSON object to string
    const fileContent = JSON.stringify(medicalHistory, null, 2);

    // Download the JSON file
    await this.downloadMedicalHistory(fileContent, `${this.patient.medicalRecordNumber}.json`);
    this.isDownloading = false;
  }

  // Helper method to format time
  private formatTime(minutes: number): string {
    if (!minutes) return 'N/A';
    const hours = Math.floor(minutes / 60);
    const mins = minutes % 60;
    return `${hours}:${mins.toString().padStart(2, '0')}`;
  }

  onCancel(): void {
    this.errorMessage = null;
    this.isSelectedMedicalRecord = false;
    this.isSelectedAppointmentHistory = false;
    history.back();
  }

  private async getPatientInformation(): Promise<Patient> {
    return new Promise((resolve, reject) => {
      this.patientService.getPatientByEmail(this.email).subscribe({
        next: (data) => {
          console.log('Patient obtained successfully', data);
          resolve(data);
        },
        error: (err) => {
          console.error('Error obtaining Patient', err);
          this.errorMessage = 'Error while obtaining Patient: ' + err.error.message;
          reject(err);
        }
      });
    });
  }

  private async getMedicalRecord(medicalRecordNumber: string): Promise<MedicalRecord> {
    return new Promise((resolve, reject) => {
      this.patientService.getMedicalRecordByPatientId(medicalRecordNumber).subscribe({
        next: (data) => {
          console.log("Medical Record obtained successfully", data);
          resolve(data);
        },
        error: (err) => {
          console.error('Error obtaining Medical Record', err);
          this.errorMessage = 'Error while obtaining Medical Record: ' + err.error.message;
          reject(err);
        }
      });
    });
  }

  private async getAppointmentHistoryByPatient(medicalRecordNumber: string): Promise<AppointmentTable[]> {
    return new Promise((resolve, reject) => {
      this.appointmentService.getAppointmentByMedicalRecordNumber(medicalRecordNumber).subscribe({
        next: (data) => {
          console.log('Appointments obtained successfully', data);
          resolve(data);
        },
        error: (err) => {
          this.errorMessage = 'Error while obtaining Appointments: ' + err.error.message;
          reject(err);
        }
      });
    });
  }

  private async downloadMedicalHistory(fileContent: string, medicalRecordNumber: string) {
    const blob = new Blob([fileContent], { type: 'text/plain' });
    const url = window.URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `medical-history-${medicalRecordNumber}`;
    a.click();
    window.URL.revokeObjectURL(url);
  }
}
