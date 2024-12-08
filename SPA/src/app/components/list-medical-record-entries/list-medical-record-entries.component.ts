import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FilterMedicalRecordEntriesComponent } from '../filter-medical-record-entries/filter-medical-record-entries.component';
import { ActivatedRoute, Router } from '@angular/router';
import { MessageService } from '../../services/message.service';
import { PatientService } from '../../services/patient.service';
import { MedicalRecord, Patient, PatientsView } from '../../models/patient';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';

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
  }

  onFilterChanged(filter: { name: string }): void {
    if (!this.medicalRecordNumber) {
      this.patientService.filterMedicalRecordEntries(this.medicalRecordNumber, filter.name).subscribe({
        next: (value: MedicalRecord) => {
          this.medicalRecord = value;
        },
        error: (err: any) => {
          console.error('Failed to filter medical record entries', err);
        }
      });
    }
  }

  updateMedicalRecord() {
    // To be implemented
  }

}
