import { CommonModule } from '@angular/common';
import { Component, EventEmitter, Output } from '@angular/core';
import { FormsModule } from '@angular/forms';

@Component({
  selector: 'app-filter-patients',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './filter-patients.component.html',
  styleUrl: './filter-patients.component.css'
})
export class FilterPatientsComponent {
  filter = {
    medicalRecordNumber: '',
    name: '',
    email: '',
    dateOfBirth: ''
  };

  @Output() filterChanged = new EventEmitter<{ medicalRecordNumber: string, name: string, email: string, dateOfBirth: string }>();

  applyFilter(): void {
    this.filterChanged.emit({
      ...this.filter,
      dateOfBirth: this.filter.dateOfBirth ? new Date(this.filter.dateOfBirth).toISOString() : ''
    });
  }

  onSearch(event: Event): void {
    event.preventDefault();
    this.applyFilter();
  }
}
