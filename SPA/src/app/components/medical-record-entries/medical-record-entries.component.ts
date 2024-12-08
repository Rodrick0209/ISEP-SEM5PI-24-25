import { Component, OnInit } from '@angular/core';
import { ListMedicalRecordEntriesComponent } from '../list-medical-record-entries/list-medical-record-entries.component';
import { ActivatedRoute } from '@angular/router';

@Component({
  selector: 'app-medical-record-entries',
  standalone: true,
  imports: [ListMedicalRecordEntriesComponent],
  templateUrl: './medical-record-entries.component.html',
  styleUrl: './medical-record-entries.component.css'
})
export class MedicalRecordEntriesComponent {
  medicalRecordEntries: string[] = [];
  medicalRecordNumber: string = '';

  constructor(private route: ActivatedRoute) { }

  onMedicalRecordEntryAdded(newMedicalRecordEntry: string) {
    this.medicalRecordEntries.push(newMedicalRecordEntry); // Adiciona novo registro médico à lista
  }
}
