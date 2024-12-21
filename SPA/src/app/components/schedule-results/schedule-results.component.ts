import { Component, OnInit } from '@angular/core';
import { CommonModule, Location } from '@angular/common';
import { Schedule, OperationSegment, ScheduleGeneticResult } from '../../services/planning.service';

@Component({
  selector: 'app-schedule-results',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './schedule-results.component.html',
  styleUrls: ['./schedule-results.component.css']
})
export class ScheduleResultsComponent implements OnInit {
  schedule: ScheduleGeneticResult | null = null;  // Dados de agendamento recebidos

  constructor(private location: Location) {}

  ngOnInit() {
    // Recebe os dados do agendamento via estado da navegação (location.state)
    this.schedule = history.state.schedule;

    if (!this.schedule) {
      console.error('No schedule data found.');
    }
  }

  goBack() {
    this.location.back();  // Navega para a página anterior
  }

  formatTime(minutes: number): string {
    const hours = Math.floor(minutes / 60);
    const mins = minutes % 60;
    return `${hours.toString().padStart(2, '0')}:${mins.toString().padStart(2, '0')}`;
  }
}
