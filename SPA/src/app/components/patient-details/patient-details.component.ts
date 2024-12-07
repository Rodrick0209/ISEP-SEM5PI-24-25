import { Component, OnInit } from '@angular/core';
import { PatientService } from '../../services/patient.service';
import { ActivatedRoute } from '@angular/router';
import { CommonModule } from '@angular/common';
import { Patient } from '../../models/patient';
import { MedicalRecord } from '../../models/patient';

@Component({
  selector: 'app-patient-details',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './patient-details.component.html',
  styleUrl: './patient-details.component.css'
})
export class PatientDetailsComponent implements OnInit {
  patient: Patient | undefined;
  medicalRecord: MedicalRecord | undefined;
  errorMessage = '';

  constructor(private patientService: PatientService, private route: ActivatedRoute) { }

  ngOnInit(): void {
    const medicalRecordNumber = this.route.snapshot.paramMap.get('medicalRecordNumber');
    if (medicalRecordNumber) {
      this.getPatientDetails(medicalRecordNumber);
      this.getPatientMedicalReport(medicalRecordNumber);
    } else {
      this.errorMessage = 'Invalid patient';
    }
    

  }

  getPatientDetails(medicalRecordNumber: string): void {
    this.errorMessage = '';

   this.patientService.getPatientByMedicalRecordNumber(medicalRecordNumber).subscribe({
      next: (data: Patient) => this.patient = data,
      error: (err: any) => {
        console.log('Failed to fetch patient details', err);
        this.errorMessage = 'Failed to fetch patient details';
      }
    });
  };

  getPatientMedicalReport(medicalRecordNumber: string): void {
    this.errorMessage = '';

    this.patientService.getMedicalRecordByPatientId(medicalRecordNumber).subscribe({
      next: (data: MedicalRecord) => {
        this.medicalRecord = data;
        // Handle the medical report data as needed
      },
      error: (err: any) => {
        console.log('Failed to fetch medical report', err);
        this.errorMessage = 'Failed to fetch medical report';
      }
    });
  }





}
