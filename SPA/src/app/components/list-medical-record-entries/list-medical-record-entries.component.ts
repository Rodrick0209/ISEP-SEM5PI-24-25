import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FilterMedicalRecordEntriesComponent } from '../filter-medical-record-entries/filter-medical-record-entries.component';
import { ActivatedRoute, Router } from '@angular/router';
import { MessageService } from '../../services/message.service';
import { PatientService } from '../../services/patient.service';
import { MedicalCondition, MedicalRecord, Patient, PatientsView } from '../../models/patient';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';
import { code } from 'three/webgpu';

@Component({
  selector: 'app-list-medical-record-entries',
  standalone: true,
  imports: [CommonModule, FormsModule, FilterMedicalRecordEntriesComponent],
  templateUrl: './list-medical-record-entries.component.html',
  styleUrl: './list-medical-record-entries.component.css'
})
export class ListMedicalRecordEntriesComponent implements OnInit {
  successMessage: string | null = null;
  errorMessage: string | null = null;
  medicalRecord: MedicalRecord | undefined;
  medicalRecordNumber: string = '';

  constructor(private patientService: PatientService, private router: Router, private messageService: MessageService, private route: ActivatedRoute) { }

  ngOnInit(): void {
    this.successMessage = this.messageService.getMessage();
    this.medicalRecordNumber = this.route.snapshot.paramMap.get('medicalRecordNumber') || ''
    this.getMedicalRecord();
  }

  onFilterChanged(filter: { designation: string }): void {
    
    if (this.medicalRecordNumber) {
      this.patientService.filterMedicalRecordEntries(this.medicalRecordNumber, filter.designation).subscribe({
        next: (value: MedicalRecord) => {
          console.log(value);
          this.medicalRecord = value;
        },
        error: (err: any) => {
          console.error('Failed to filter medical record entries', err);
        }
      });
    }
  }

  getMedicalRecord(): void {
    if (this.medicalRecordNumber) {
      this.patientService.getMedicalRecordByPatientId(this.medicalRecordNumber).subscribe({
        next: (record: MedicalRecord) => {
          this.medicalRecord = record;
        },
        error: (err: any) => {
          console.error('Failed to get medical record', err);
          this.errorMessage = 'Failed to get medical record';
        }
      });
    }
  }

  updateMedicalRecord() {
    if (this.medicalRecordNumber) {
      this.router.navigate(['/edit-medical-record', this.medicalRecordNumber]);
    }
    
  }


}
